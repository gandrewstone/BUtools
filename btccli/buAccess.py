import pdb
import types
import fnmatch
import httplib
import socket
import time
import json
from parallelize import *
import bitcoin
import bitcoin.rpc as btc
import urlparse

MAX_SAMPLES = 1000
Top = None

def xmlElemify(s):
  """Transform the input string if it is not a valid XML tag"""
  s = s.replace(":","_")
  if s[0].isdigit():
    return "_" + s
  else: return s
          
def mgtGet(xpath):
  print xpath
  xpl = xpath.split("/")
  depth=8
  if xpl[0][0:1] == "{":
     args = xpl[0][1:-1].split(",") # [1:-1] chops off the surrounding {}
     for a in args:
       if a[0]=="d":
         (d,depthStr) = a.split("=")
         depth = int(depthStr)
  elif xpl[0] != '':  # xpath does not begin with a / but relative paths should be resolved in higher layer SW because there is not concept of current dir here
    print "Path is: %s" % str(xpl)
    assert(0)
  xml = Top.resolveToXml(xpl[1:],depth)
  return xml

def isValidDirectory(path):
  data = mgtGet(str("{d=0}"+path))
  if not data: return False  # TODO, what about an empty list?
  # print "isValidDir: ", str(data)
  # pdb.set_trace()
  return True

class Commands:
  def __init__(self,config):
    self.commands = {}
    self.context = None
    self.config = config

  def canonicalPath(self,location):
    print "cur: ", self.context.curdir, " loc: ", location
    if location == "." or location == "" or location == None:
      return self.context.curdir
    if location[0] == "/": # not a relative path
      return location
    if self.context.curdir == "/":
      return "/" + location
    elif self.context.curdir:
      return self.context.curdir + "/" + location
    return location
 

  def do_connect(self, name, location=None, username=None, password=None):
    """syntax: 
        connect (name) [ip:port] [username] [password] 
        Connects to a Bitcoin Unlimited instance.  If just a name is given, the local instance will be contacted
    """
    global Top
    if location:  # No location given is localhost
      if ":" in location:    
        (location,port) = location.split(":")
      else:
        port = 8332 # Bitcoin rpc port
    else:
      port = None

    # ip=None,port=None,user=None,password=None
    server = Server(name,location,port,username, password)  # TODO
    Top.add(server)
    if not self.config.has_section("Servers"): self.config.add_section("Servers")
    self.config.set("Servers",name,json.dumps([name,location,port,username,password]))
    return str("ok")

  def setContext(self,context):
    """Sets the context (environment) object so commands can access it while they are executing"""
    self.context = context

class XmlNode:
  def __init__(self,name,parent=None):
    self.name = name
    self.data = None
    self.parent = parent
    self.children = {}
    self.attributes = {}

  def fullPath(self,separator='/'):
    if self.parent: return self.parent.fullPath() + separator + self.name
    else: return separator

  def toXml(self, depth,fullPathOnce=False):
        if self.data or self.children and depth > 0:
          full = 1
        else: full = 0

        if self.children:
          chlst=[]
          if depth:
            for (k,c) in self.children.items():
              if isinstance(c,XmlNode):
                chlst.append(c.toXml(depth-1))
              else:
                if k:
                  chlst.append("<%s>%s</%s>" % (str(k),str(c),str(k)))
                else:
                  chlst.append(str(c))
          if chlst: full = 1  # If there's something in the child list, generate full xml
          chstr = "".join(chlst)
        else: chstr = ""

        attrStringLst = []
        if len(self.attributes)>1:
          # Format attribute string
          attrStringLst = ["%s='%s'" % (k,v) for (k,v) in self.attributes.items()]
        else:
          attrStringLst = []
        if fullPathOnce:
          attrStringLst.append("path='%s'" % self.fullPath())
        attrs = " " + " ".join(attrStringLst)

        if full:
          data = self.data if self.data else ""
          return "<%s%s>%s%s</%s>" % (self.name,attrs,str(data),chstr,self.name)
        else:
          return "<%s%s />" % (self.name,attrs)

  def resolveToXml(self, path,depth=8):
      elementList = self.resolve(path)
      if not elementList:
        return "<error>Unknown path [%s]</error>" % "/".join(path)

      ret = alltogether([lambda p=e,d=depth: p.toXml(d,fullPathOnce=True) for e in elementList])
#      ret = []      
#      for e in elementList:
#        ret.append(e.toXml(depth,fullPathOnce=True))
      ret = [ x for x in ret if x is not None]
      return "\n".join(ret)

  def matches(self,spec):
      if self.name == spec[0]:
        return spec[1:]
      if fnmatch.fnmatch(self.name,spec[0]):
        return spec[1:]
      if spec[0] == "**":  
        return spec # ** matches many children so spec node is not consumed
      if spec[0][0] == "{":
        choices = [ x.strip() for x in spec[0][1:-1].split(",")]
        for ch in choices:
            if fnmatch.fnmatch(self.name, ch):
                return spec[1:]
        #f self.name in choices:
        #   return spec[1:]
      #if "*" in spec[0]:
      #  pdb.set_trace()          
      return None
 
  def resolve(self,path): 
      if not path:  # Nothing left so return this object
        return [self]
      else:
        mychildname = path[0]
        if mychildname == "" or mychildname == ".":
          return self.resolve(path[1:]) # foo//bar == foo/bar and also handle foo/, foo/./bar == foo/bar
        if mychildname == "..":
          next = self.parent if self.parent else self  # .. goes up but not beyond the top
          return next.resolve(path[1:])

        if self.children.has_key(mychildname): # Try a direct lookup
          ch = self.children[mychildname]
          try:
            return ch.resolve(path[1:])
          except:  # its a raw number
            pdb.set_trace()
            return ch
          
        # Lookup did not work, iterate thru all children looking for a complex match
        if 1:
          ret = []
          for (k,c) in self.children.items():
            if isinstance(c,XmlNode):  # Make sure its a node, not just data
              rest = c.matches(path)
              if rest is not None:  # If [] is returned, that should eval to True.  It means that that match fully consumed the path
                ret += c.resolve(rest)
          return ret


  def add(self,child):
    self.children[child.name] = child
    child.parent = self

  def remove(self, name):
    del self.children[name]

HistoryIntervals = ["total","now","sec10","min5","hourly","weekly","monthly"]

class StatHistory(XmlNode):
  def __init__(self,name):
    XmlNode.__init__(self,name)

    for x in HistoryIntervals: 
      self.add(StatHistoryLeaf(x))

class StatHistoryLeaf(XmlNode):
  def __init__(self,name):
    XmlNode.__init__(self,name)

  def getAccessData(self):
    svr = self.parent
    fname = []
    while not isinstance(svr, Server):
      fname.insert(0,svr.name)
      svr=svr.parent
    fname = "/".join(fname)
    return (svr,fname)

  def update(self,data):
    if type(data) is types.DictType:
      for (k,v) in data.items():
        if type(v) == types.DictType:
          assert(0); # TODO recursion
        self.children[k] = v
    elif type(data) is types.ListType:
      self.data = ",".join([ json.dumps(x) for x in data])
    else:
      self.data = data

  def refresh(self):
    (server,fname) = self.getAccessData()
    name = self.name
    result = server.call("getstat",fname,name,MAX_SAMPLES)
    if not result:
      self.parent.remove(self.name)
      return False
    else:
      result = result[0][name]  # Unwrap it
      self.update(result)
    return True

  def toXml(self, depth,fullPathOnce=False):
    if depth>0:
      if not self.refresh():  # element is gone
        return ""
    return XmlNode.toXml(self,depth,fullPathOnce)

class Branch(XmlNode):
  def __init__(self,name):
    XmlNode.__init__(self,name)
  

class Server(XmlNode):
  def __init__(self,name,ip=None,port=None,user=None,password=None):
    XmlNode.__init__(self,name)
    self.host = ip
    self.port = port
    self.user = user
    self.password = password
    self.server=None
    self.reconnect()
    try:
      result = self.call("getstatlist")
      self.createStatTree(result)
      self.data = None
    except bitcoin.rpc.JSONRPCError, e:
      pass
    except socket.error:
      pass

  def reconnect(self):
    if self.server:
      del self.server
    if self.host is None:
      self.server = btc.Proxy()
    else:
      self.server = btc.Proxy("http://"+str(self.host)+"/",service_port=self.port)
      self.server.setConnParams(self.host, self.port, self.user, self.password)
      #service_url = 'http://%s:%s@%s:%d' % (self.user, self.password, self.host, self.port)
      #self.server.__service_url = service_url
      #self.server__url = urlparse.urlparse(service_url)

  def call(self,*cmdargs):
    result = ""
    errorCount = 0
    while True:
      try:
        result = self.server._call(*cmdargs)
        break
      except httplib.BadStatusLine:
        time.sleep(.1)
      except httplib.CannotSendRequest:
        time.sleep(.1)
        self.reconnect()
      except bitcoin.rpc.JSONRPCError, e:
        self.data = e.error["message"]
        raise  # TODO not sure what to do here
      except socket.error, e:
        # pdb.set_trace()
        if e[0] == 'timed out':
          raise            
        else: self.data = e[1]
        if errorCount > 5: raise
        time.sleep(.1)
      errorCount+=1

    return result

  def convertStatTree(self,tree,parent):
    if not tree: return None

    for (k,v) in tree.items():
      if not v: 
        node = StatHistory(k)
      else:
        node = Branch(k)
        self.convertStatTree(v,node)
      parent.add(node)

  def createStatTree(self,statList):
    statListSplit=[ x.split("/") for x in statList]
    tree = {}

    # Make a nested tree structure
    for s in statListSplit:
      pos = tree
      for i in s:
        i = xmlElemify(i)
        if not pos.has_key(i):
          pos[i] = {}
	pos = pos[i]

    # convert and insert
    self.convertStatTree(tree,self)
        
    

def Connecter(name,location,port,username, password):
    return Server(name,location,port,username, password)

def Initialize(configuration):
  global Top
  Top = XmlNode("root",None)
  #Top.add(Server("local"))

  if configuration.has_section("Servers"):
    connectFns = []
    for key, value in configuration.items("Servers"):
      (name, location,port,username, password) = json.loads(value)
      connectFns.append((Connecter,name, location,port,username, password))
      #server = Server(name,location,port,username, password)
      #Top.add(server)
  ret = alltogether(connectFns)
  for r in ret: Top.add(r)

  return Commands(configuration),{}
