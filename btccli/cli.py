#!/usr/bin/env python
import sys, os, os.path, time, types
import argparse
import json
import ConfigParser as configparser

access = None
CliName = ''

import buAccess as access

NodeColor = (0xc0,0xc0,0xf0)
LeafColor = (0xff,0xc0,0xff)

FullPathColor = (0xc0,0xa0,0)
DEFAULT_WINDOW_SIZE = (600,900)
CONFIG_FILE = os.path.expanduser("~/.xmlterm.cfg")

import pdb
import xml.etree.ElementTree as ET

import xmlterm
windowed = True

def formatTag(tag):
  """Take a tag and format it for display"""
  if tag[0] == "{":  # Get rid of the namespace indicator b/c that's ugly
    tag = tag.split("}")[1]
  return tag
  

def commonSuffix(stringList):
  """Given a list of strings, return the string that is the common ending of all strings in the list"""
  revList = [ s[::-1] for s in stringList]
  revPfx = os.path.commonprefix(revList)
  return revPfx[::-1]

# Handlers take XML ElementTree objects and turn them into displayable objects


def dumpNoTagHandler(elem, resolver,context):
  """ """
  resolver.add(xmlterm.FancyText(resolver.parentWin, elem.text))
  

def defaultHandler(elem,resolver,context):
  """Handle normal text and any XML that does not have a specific handler"""
  # Name preference order is: "key" attribute of tag (for lists) > "name" attribute of tag > "name" child of tag > tag 
  nameChild = elem.find("name")
  if nameChild is not None: name=nameChild.text
  else: name=elem.tag
  name = elem.attrib.get("name",elem.tag) 
  name = formatTag(name) # Get rid of the namespace indicator b/c that's ugly
  # ns = 

  fore = None
  #more = elem.find("more")
  #if more is None:
  #  more = elem.find("{%s}more" % SAFplusNamespace)
  #if more is not None:

  if elem._children:  # it will either have child nodes or a "more" child node if it has children
    fore = NodeColor
  else:
    fore = LeafColor

  if elem.text:
    top = name + ":" + elem.text
  else:
    top = name

  w = xmlterm.FancyTextChunked(resolver.parentWin, ("  "*resolver.depth) + top,chunkSize=2048,fore=fore)
  resolver.add(w)  

  resolver.depth += 1
  context.path.append(elem.tag)
  for child in elem:
    resolver.resolve(child,context)
    if child.tail and child.tail.strip():
      w = xmlterm.FancyTextChunked(resolver.parentWin,("  "*resolver.depth) + child.tail,chunkSize=2048)
      resolver.add(w)
  del context.path[-1]   
  resolver.depth -= 1


def defaultTextHandler(elem,resolver,context):
  """Handle normal text and any XML that does not have a specific handler"""
  # Name preference order is: "key" attribute of tag (for lists) > "name" attribute of tag > "name" child of tag > tag 
  childReorg(elem,context.path)
  nameChild = elem.find("name")
  if nameChild is not None: name=nameChild.text
  else: name=elem.tag
  name = elem.attrib.get("name",elem.tag) 
  name = elem.attrib.get("listkey", name)
  name = formatTag(name) # Get rid of the namespace indicator b/c that's ugly
  fore = None

  if elem._children:  # it will either have child nodes or a "more" child node if it has children
    fore = NodeColor
  else:
    fore = LeafColor

  if elem.text:
    top = name + ":" + elem.text
  else:
    top = name

  w = ("  "*resolver.depth) + top
  resolver.add(w)  

  resolver.depth += 1
  context.path.append(elem.tag)
  for child in elem:
    resolver.resolve(child,context)
    if child.tail and child.tail.strip():
      w = ("  "*resolver.depth) + child.tail
      resolver.add(w)
  del context.path[-1]   
  resolver.depth -= 1


def topHandler(elem,resolver,context):
  """Make this XML node invisible -- skip down to the children"""
  # childReorg(elem,context.path)
  for child in elem:
    fullpath = child.attrib.get("path",None)
    if fullpath: # print out the full path with a : if it is placed into the child's attribute list.  This only happens for top level nodes so the user can see where they were found
      resolver.add(xmlterm.FancyText(resolver.parentWin,fullpath + ":",fore=FullPathColor))      
      resolver.depth+=1
    resolver.resolve(child,context)
    if fullpath:
      resolver.depth-=1

    if child.tail and child.tail.strip():
      w = xmlterm.FancyTextChunked(resolver.parentWin,("  "*resolver.depth) + child.tail,chunkSize=2048)
      resolver.add(w)
    

def childrenOnlyHandler(elem,resolver,context):
  """Make this XML node invisible -- skip down to the children"""
  childReorg(elem,context.path)
  for child in elem:
    resolver.resolve(child,context)
    if child.tail and child.tail.strip():
      w = xmlterm.FancyTextChunked(resolver.parentWin,("  "*resolver.depth) + child.tail,chunkSize=2048)
      resolver.add(w)

def historyHandler(elem,resolver,context):
  """Create a plot for the historical statistics XML nodes"""
  if elem.text:
    data = [float(x) for x in elem.text.split(",")]
    title = elem.attrib.get("title", formatTag(elem.tag))
    xlabel = elem.attrib.get("xlabel", "time")
    ylabel = elem.attrib.get("ylabel", "")  # TODO: grab the parent's tag as the ylabel?
    
    plotAttrib = { "title":title, "xlabel":xlabel, "ylabel":ylabel}
    if len(resolver.windows)>1:  # If there is lots of other data in this command, show a small graph
      plotAttrib["size"]="(200,100)"
    plot = ET.Element("plot",plotAttrib)
    series = ET.Element("series", {"label":""},text=elem.text)
    series.text = elem.text
    plot.append(series)
    resolver.add(xmlterm.FancyText(resolver.parentWin,formatTag(elem.tag)))
    w = xmlterm.PlotPanel(resolver.parentWin,plot)
    resolver.add(w)
  else:
    resolver.defaultHandler(elem,resolver,context)


def serviceUnitHandler(elem,resolver,context):
  """Custom representation of a service unit"""
  name = elem.attrib.get("listkey",formatTag(elem.tag))
  w = xmlterm.FancyText(resolver.parentWin,("  "*resolver.depth) + name,fore=SuNameColor,size=SuNameSize)
  resolver.add(w)
  try:
    w = xmlterm.FancyText(resolver.parentWin, ("  "*resolver.depth) + "  Admin: %s Operational: %s %s " % (elem.find("adminState").text,"OK" if elem.find("operState").text == "1" else "FAULTED", elem.find("haState").text))
    resolver.add(w)
    w = xmlterm.FancyText(resolver.parentWin, ("  "*resolver.depth) + "  Active Work: %s Standby Work: %s" % (elem.find("numActiveServiceInstances").find("current").text,elem.find("numStandbyServiceInstances").find("current").text))
    resolver.add(w)
    w = xmlterm.FancyText(resolver.parentWin, ("  "*resolver.depth) + "  On Node: %s  In Service Group: %s" % (elem.find("node").text.split("/")[-1],elem.find("serviceGroup").text.split("/")[-1]))
    resolver.add(w)
  except AttributeError, e: # object could have no children because recursion depth exceeded
    pass 

def serviceUnitListHandler(elem,resolver,context):
  """Create a graphical representation of the XML 'text' tag"""
  if elem.attrib.has_key("listkey"): # Its an instance of the list
    serviceUnitHandler(elem,resolver,context)
    return
  path = elem.attrib.get("path",None)
  if path == "/SAFplusAmf/ServiceUnit" or len(elem)>0:  # serviceunit tag can be a list, a list entry, or a su ref.  SU ref has no children
    resolver.add(xmlterm.FancyText(resolver.parentWin,("  "*resolver.depth) + "ServiceUnit",fore=SuListColor,size=SuListSize))
    resolver.depth += 1
    for su in elem:
      serviceUnitHandler(su,resolver,context)
    resolver.depth -= 1
  else:  # Could be ServiceUnit has no children because recursion depth exceeded
    w = xmlterm.FancyText(resolver.parentWin,("  "*resolver.depth) + formatTag(elem.tag))  # so normal display
    resolver.add(w)
    
  
  if elem.text:  # There should not be any text in the service unit list but display it if there is
    size = elem.attrib.get("size",None)
    if size: size = int(size)
    fore = elem.attrib.get("fore",None)
    if fore: fore = color.rgb(fore) # [ int(x) for x in fore.split(",")]
    back = elem.attrib.get("back",None)
    if back: back = color.rgb(back) # [ int(x) for x in back.split(",")]
    w = xmlterm.FancyText(resolver.parentWin,elem.text,fore=fore, back=back,size=size)
    resolver.add(w)

seriesTags = ["sec10","min5","daily","hourly","monthly"]

def findSeries(et,prefix=None):
  """Pull all plottable elements out of the element tree and return them with a series label.  A series is a list of numbers"""
  if prefix is None: 
    prefix = ""
  if et.attrib.has_key("path"):  # override the prefix with a supplied path if it exists, since that is guaranteed to be complete
    prefix = et.attrib["path"]
  else:
    prefix = prefix + "/" + et.tag  # Build the prefix from the et structure
  if et.tag in seriesTags:
      return [(prefix, et.text)]
  ret = []
  for e in et:
    t = findSeries(e,prefix)
    if t: ret += t
  return ret

def findMetrics(et,prefix=None):
  """Pull all plottable elements out of the element tree and return them with a metric label.  A metric is just a single number, while a series is a list of numbers"""
  if prefix is None: 
    prefix = ""
  if et.attrib.has_key("path"):  # override the prefix with a supplied path if it exists, since that is guaranteed to be complete
    prefix = et.attrib["path"]
  else:
    prefix = prefix + "/" + et.tag  # Build the prefix from the et structure
  ret = []
  try:  # If we can convert into a number, report it as a metric
    test = float(et.text)
    ret.append((prefix, test))
  except Exception, e:
    try:  # If we are given a list of numbers then use that
      test = [ float(x) for x in et.text.split(",")]
      ret.append((prefix, test))
    except Exception, e:
      pass
    pass
  for e in et:
    t = findMetrics(e,prefix)
    if t: ret += t
  return ret

def uniquePortion(seriesNames):
    """Determine the common prefix and suffix of a list of names, and return (x,y) such that name[x:y] removes the common parts"""
    commonPfx = os.path.commonprefix(seriesNames)
    t = commonPfx.rfind("/")  # we want to split across / boundaries, not within tags. that is: commonprefix([a/bc, a/bd] -> a/b  so convert that to "a/"
    if t>=0: commonPfx = commonPfx[0:t+1]
    pfxLen=len(commonPfx)
 
    commonSfx = commonSuffix(seriesNames)
    t = commonSfx.find("/")
    if t>=0: commonSfx = commonSfx[t:]
    sfxLen=-1 * len(commonSfx)
    if sfxLen==0: sfxLen = None
    return (pfxLen,sfxLen)


class TermController(xmlterm.XmlResolver):
  """This class customizes the XML terminal"""
  def __init__(self):
    xmlterm.XmlResolver.__init__(self)
    self.tags.update(xmlterm.GetDefaultResolverMapping())
    self.curdir = "/"
    self.cmds.append(self)  # I provide some default commands
    self.xmlterm = None

  def newContext(self):
    path = self.curdir.split("/")
    return xmlterm.ParsingContext(path)

  def start(self,xt):
    """Called when the XML terminal is just getting started"""
    self.xmlterm = xt
    xt.frame.SetTitle(CliName)
    
  def prompt(self):
    """Returns the terminal prompt"""
    return self.curdir + "> "

  def new(self):
    """Clone this controller for sub-documents"""
    return TermController()

  def completion(self,s):
    """Return the best command line completion candidate, given that the user already typed the string s"""
    if not s:
      return ""
    cmds=["!time ", "!echo ", "alias ", "!alias ", "!name","cd","get" ]
    for c in cmds:
      if c.startswith(s):
        print "complete", c
        return c[len(s):]
    return ""

  def bar(self,xml,xt):
    """Implement the bar graph command, which draws a bar graph"""
    et = ET.fromstring("<top>" + "".join(xml) + "</top>")
    metrics = findMetrics(et)
    if not metrics: 
      xt.doc.append("Nothing to graph")
      return
    metricNames = [x[0] for x in metrics]
    (pfxLen,sfxLen) = uniquePortion(metricNames)
    st = ['<barGraph>']
    for s in metrics:
      if s[1]:  # If the series has some data then add it to the plot
        lbl = s[0][pfxLen:sfxLen]
        st.append('<metric label="%s">' % lbl)
        if type(s[1]) is types.ListType:
          st.append(str(sum(s[1])/len(s[1])))  # TODO specify the operation.  sum, max, min, ave
        else: 
          st.append(str(s[1]))
        st.append('</metric>')
    st.append("</barGraph>")
    xt.doc.append("".join(st))  # I have to wrap in an xml tag in case I get 2 top levels from mgtGet
  

  def plot(self,xml,xt):
    """Implement the plot command -- draws a plot (line graph)"""
      
    et = ET.fromstring("<top>" + "".join(xml) + "</top>")
    series = findSeries(et)
    if len(series)==0:
      xt.doc.append("Nothing to plot")
      return

    seriesNames = [x[0] for x in series]

    sz = xt.GetSize()
    sizeString = "(%s,%s)" % (sz[0]-30,sz[1]/2)
    (pfxLen,sfxLen) = uniquePortion(seriesNames)

    st = ['<plot size="%s">' % sizeString]

    for s in series:
      if s[1]:  # If the series has some data then add it to the plot
        if s[1][0] == "{":  # If its complex data just try to grab the "val" out of it
          dlst = json.loads("[" + s[1] + "]")
          series = [x["val"] for x in dlst]
        else:
          series = s[1]
        lbl = s[0][pfxLen:sfxLen]
        st.append('<series label="%s">' % lbl)
        st.append(str(series))
        st.append('</series>')
    st.append("</plot>")
    # print "".join(st)
    xt.doc.append("".join(st))  # I have to wrap in an xml tag in case I get 2 top levels from mgtGet
 
  def do_help(self,*command):
    """? display this help, or detailed help about a particular command"""
    if command:
      command = [" ".join(command)]
    else:
      command = None
    return "<top>" + self.getHelp(command) + "</top>"

  def do_ls(self,*sp):
    """Displays the object tree at the current or specified location.
Arguments: [-N] location
By default the specified location and children are shown.  Use -N to specify how many children to show.  For example -2 will show this location, children and grandchildren.
    """
    depth=1
    rest = [""]
    if len(sp):  
      if sp[0][0] == "-":  # Its a flag
        depth = int(sp[0][1:])  # depth modifier flag
        if len(sp)>1:
          rest = sp[1:]
      else:             
        rest = sp[0:]
    xml = []
    for r in rest:
      t = os.path.normpath(os.path.join(self.curdir,r))
      gs = "{d=%s}%s" % (depth,str(t))
      xml.append(access.mgtGet(gs))
    #print xml
    return "<top>" + "\n".join(xml) + "</top>"  # I have to wrap in an xml tag in case I get 2 top levels from mgtGet

  def do_cd(self,location):
    """Change the current directory (object tree context)"""
    tmp = os.path.normpath(os.path.join(self.curdir,location))
    if access.isValidDirectory(tmp):
      self.curdir = tmp
      self.xmlterm.cmdLine.setPrompt()
    else:
      return "<error>Invalid path [%s]</error>" % tmp
    return ""

  def do_pwd(self,*sp):
    """Print working directory - shows the current directory"""
    return self.curdir

  def do_raw(self,*sp):
    """Equivalent to 'ls' but displays raw XML"""
    depth=1
    prefix = "{d=%s}"
    rest = ""
    flags = sp
    for flag in flags:  
      if flag[0] == "-":  # Its a flag
        if flag[1] == 'b':
          prefix = "{b,d=%s}"
        elif flag[1] == 'p':
          prefix = "{p,d=%s}"
        else:
          try:  
            depth = int(flag[1:])  # depth modifier flag
          except ValueError, e:
            xt.doc.append("bad flag: " + flag)
            pass
      else:             
        rest = flag
    t = os.path.normpath(os.path.join(self.curdir,rest))
    # print (prefix % depth) + str(t)
    xml = access.mgtGet((prefix % depth) + str(t))
    txt = "<text>" + xmlterm.escape(xmlterm.indent("<top>" + xml + "</top>")) + "</text>"
    return txt # I have to wrap in an xml tag in case I get 2 top levels from mgtGet

  def do_time(self,*sp):
    """Show the time"""
    return "<time/>"

  def do_echo(self,*sp):
    """Print the args back at the user"""
    return " ".join(sp[1:])

  def do_title(self,title):
    """Change this window's title"""
    self.xmlterm.frame.SetTitle(title)
    return ""

  def do_alias(self,alias,*val):
    """Make one command be replaced by another string"""
    self.xmlterm.aliases[alias] = " ".join(val)
    return ""

  def do_exit(self):
    """Exit this program"""
    self.xmlterm.frame.Close()    

  def do_plot(self,*sp):
          """? Draw a line graph
    Arguments: locations that contain plottable data (comma separated list of numbers)
    Example:  Plot memory utilization on all components in 10 second and 5 minute time frames. 
       cd /local/node/*/bytesReceived
       plot sec10 min5         
"""
          depth=1
          rest = ""
          if len(sp):  
            if sp[0][0] == "-":  # Its a flag
              depth = int(sp[0][1:])  # depth modifier flag
              if len(sp)>1:
                rest = sp[1:]
            else:             
              rest = sp
          xml = []
          for arg in rest:
            t = os.path.normpath(os.path.join(self.curdir,arg))
            xml.append(access.mgtGet("{d=%s}%s" % (depth,str(t))))
          self.plot(xml,self.xmlterm)
          return ""

  def do_bar(self,*sp):
          """? Draw a bar graph.  A bar graph compares single values of multiple entities.  If locations do not resolve to single-number entities, the bar graph will not be shown.
    Arguments: locations (must contain a number)
    Example:  Compare number of threads in all components on all network elements
       cd /*/safplusAmf/Component/*/procStats
       bar numThreads/current
          """
          depth=1
          rest = ""
          if len(sp):  
            if sp[0][0] == "-":  # Its a flag
              depth = int(sp[0][1:])  # depth modifier flag
              if len(sp)>1:
                rest = sp[1:]
            else:             
              rest = sp
          xml = []
          for arg in rest:
            t = os.path.normpath(os.path.join(self.curdir,arg))
            xml.append(access.mgtGet("{d=%s}%s" % (depth,str(t))))
          self.bar(xml,self.xmlterm)
          return ""

  def xxexecute(self,textLine,xt):
    """Execute the passed string"""
    cmdList = textLine.split(";")
    depth = 1
    while cmdList:
      text = cmdList.pop(0) 
      sp = text.split()
      if sp:
        alias = xt.aliases.get(sp[0],None)
        if alias: # Rewrite text with the alias and resplit
          sp[0] = alias
          text = " ".join(sp)
          sp = text.split()
        # Now process the command
        if sp[0]=="ls" or sp[0] == "dir":
          rest = ""
          if len(sp)>1:  
            if sp[1][0] == "-":  # Its a flag
              depth = int(sp[1][1:])  # depth modifier flag
              if len(sp)>2:
                rest = sp[2]
            else:             
              rest = sp[1]
          t = os.path.normpath(os.path.join(self.curdir,rest))
          gs = "{d=%s}%s" % (depth,str(t))
          #print "getting ", gs
          xml = access.mgtGet(gs)
          #print xml
          xt.doc.append("<top>" + xml + "</top>")  # I have to wrap in an xml tag in case I get 2 top levels from mgtGet
        elif sp[0]=="plot":
          rest = ""
          if len(sp)>1:  
            if sp[1][0] == "-":  # Its a flag
              depth = int(sp[1][1:])  # depth modifier flag
              if len(sp)>2:
                rest = sp[2:]
            else:             
              rest = sp[1:]
          xml = []
          for arg in rest:
            t = os.path.normpath(os.path.join(self.curdir,arg))
            xml.append(access.mgtGet("{d=%s}%s" % (depth,str(t))))
          self.plot(xml,xt)
        elif sp[0]=="bar":
          rest = ""
          if len(sp)>1:  
            if sp[1][0] == "-":  # Its a flag
              depth = int(sp[1][1:])  # depth modifier flag
              if len(sp)>2:
                rest = sp[2:]
            else:             
              rest = sp[1:]
          xml = []
          for arg in rest:
            t = os.path.normpath(os.path.join(self.curdir,arg))
            xml.append(access.mgtGet("{d=%s}%s" % (depth,str(t))))
          self.bar(xml,xt)
        
        elif sp[0]=="cd":
          self.curdir = os.path.normpath(os.path.join(self.curdir,sp[1]))
          xt.cmdLine.setPrompt()
        elif sp[0]=='pwd':
          xt.doc.append(self.curdir)
        elif sp[0]=='raw':
          prefix = "{d=%s}"
          rest = ""
          flags = sp[1:]
          for flag in flags:  
            if flag[0] == "-":  # Its a flag
              if flag[1] == 'b':
                prefix = "{b,d=%s}"
              elif flag[1] == 'p':
                prefix = "{p,d=%s}"
              else:
                try:  
                  depth = int(flag[1:])  # depth modifier flag
                except ValueError, e:
                  xt.doc.append("bad flag: " + flag)
                  pass
            else:             
              rest = flag
          t = os.path.normpath(os.path.join(self.curdir,rest))
          # print (prefix % depth) + str(t)
          xml = access.mgtGet((prefix % depth) + str(t))
          txt = "<text>" + xmlterm.escape(xmlterm.indent("<top>" + xml + "</top>")) + "</text>"
          xt.doc.append(txt)  # I have to wrap in an xml tag in case I get 2 top levels from mgtGet

        elif sp[0]=="!time": # Show the time (for fun)
          xt.doc.append("<time/>")
        elif sp[0] == '!echo':  # Display something on the terminal
          xt.doc.append(" ".join(sp[1:]))
          if 0:
           try:  # If its good XML append it, otherwise excape it and dump as text
            rest = " ".join(sp[1:])
            testtree = ET.fromstring(rest)
            xt.doc.append(rest)
           except ET.ParseError:
            xt.doc.append(xmlterm.escape(rest))
        elif sp[0] == '!name':  # Change the terminal's title
          xt.frame.SetTitle(sp[1])
        elif sp[0] == 'alias' or sp[0] == '!alias':  # Make one command become another
          xt.aliases[sp[1]] = " ".join(sp[2:])
        elif sp[0] == '!exit' or sp[0] == 'exit':  # goodbye
          self.parentWin.GetParent().frame.Close()
        else:
          pdb.set_trace()
          # TODO look for registered RPC call
          t = xmlterm.escape(" ".join(sp))
          xt.doc.append('<process>%s</process>' % t)  



def main(args):
  global access, CliName

  parser = argparse.ArgumentParser()
  parser.add_argument("-l", "--local", action="store_true", help=('access the local client only'))
  args = parser.parse_args()

  if args.local:
    pass    

  config = configparser.SafeConfigParser()
  ret = config.read(CONFIG_FILE)  
  print ret

  cmds,handlers = access.Initialize(config)

  if windowed:
    os.environ["TERM"] = "XT1" # Set the term in the environment so child programs know xmlterm is running
    resolver = TermController()
    resolver.addCmds(cmds)
    # resolver.tags["ServiceUnit"] = serviceUnitListHandler
    resolver.tags["root"] = topHandler
    resolver.tags["top"] = topHandler
    resolver.tags["more"] = childrenOnlyHandler  # don't show this indicator that the node has children
    # resolver.tags["bootTime"] = epochTimeHandler
    # resolver.tags["lastInstantiation"] = epochMsTimeHandler
    # resolver.tags["upTime"] = elapsedSecondsHandler
    # resolver.tags["pendingOperationExpiration"] = epochMsTimeHandler

    # resolver.tags["history10sec"] = historyHandler
    #resolver.tags["history1min"] = historyHandler
    #resolver.tags["history10min"] = historyHandler
    #resolver.tags["history1hour"] = historyHandler
    #resolver.tags["history1day"] = historyHandler
    #resolver.tags["history1week"] = historyHandler
    #resolver.tags["history4weeks"] = historyHandler

    resolver.tags.update(handlers)

    resolver.depth = 0
    resolver.defaultHandler = defaultHandler
    doc = []

    try:
      try:
        size = tuple(json.loads(config.get("LookAndFeel","size")))
      except configparser.NoOptionError:
        size = DEFAULT_WINDOW_SIZE
      try:
        position = tuple(json.loads(config.get("LookAndFeel","position")))
      except configparser.NoOptionError:
        position = None
    except configparser.NoSectionError:
      size=DEFAULT_WINDOW_SIZE
      position = None


    app = xmlterm.App(lambda parent,doc=doc,termController=resolver,config=config: xmlterm.XmlTerm(parent,doc,termController=resolver,config=config),redirect=False,size=size, position=position)
    app.MainLoop()
  
    fp = open(CONFIG_FILE,"w")
    ret = config.write(fp)  
    fp.close()
   
  else:
    resolver = TermController()
    resolver.xmlterm = resolver # just dumping everything in one class
    resolver.defaultHandler = defaultTextHandler
    resolver.tags["bootTime"] = epochTimeHandler
    resolver.tags["lastInstantiation"] = epochMsTimeHandler
    resolver.tags["upTime"] = elapsedSecondsHandler
    resolver.tags["pendingOperationExpiration"] = epochMsTimeHandler
    resolver.tags["top"] = topHandler
    resolver.tags["more"] = childrenOnlyHandler  # don't show this indicator that the node has children
    resolver.tags["text"] = dumpNoTagHandler 
    resolver.addCmds(cmds)
    while 1:      
      cmd = resolver.cmdLine.input()
      resolver.execute(cmd,resolver)



if __name__ == '__main__':
    main(sys.argv)


def Test():
  main([])
