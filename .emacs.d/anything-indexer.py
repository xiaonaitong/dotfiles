#! /usr/bin/env python
# -* - coding: UTF-8 -* -
import sys
from bs4 import BeautifulSoup
from urllib2 import urlopen, HTTPError

def javadoc_class_index(uri_prefix):
    response = urlopen("%sallclasses-frame.html" % uri_prefix)
    soup = BeautifulSoup(response.read(), 'lxml')

    links = [link.get('href') for link in soup.find_all('a')]

    result = []

    for link in links:
        class_name = link.replace("/", ".")[:-5] # trim ".html"
        result.append([class_name, link])
    return result

def _href_to_name(href):
    return href.split("/")[-1].replace(".html", "").replace("#", ".")

def _index_resource(uri):
    try:
        response = urlopen(uri)
    except HTTPError:
        return None

    result = []
    content = response.read()
    soup = BeautifulSoup(content, 'lxml')
    for dt in soup.find_all('dt'):
        href = dt.a.get('href')
        if "#" in href:
            result.append([_href_to_name(href), href])
    return result

def javadoc_detail_index(uri_prefix):
    index_all = "%sindex-all.html" % uri_prefix
    index_az = ["%sindex-files/index-%s.html" % (uri_prefix, i)
                for i in range(1, 28)]

    # index-all may not exist
    result = _index_resource(index_all)
    if result:
        return result
    
    return [i for uri in index_az
              for i in _index_resource(uri)]

def _normalize_method_name(name):
    name = name.strip()
    if name.startswith('.'):
        name = name[1:]
    if name.endswith('()'):
        name = name[:-2]
    return name

def _normalize_description(content):
    content = content.strip()
    return content

# for index jquery
def jquery_index(uri):
    result = []
    response = urlopen(uri)
    
    soup = BeautifulSoup(response.read().decode("utf-8"), 'lxml')
    for i in soup.find_all("article", class_="hentry"):
        link = i.find(class_='entry-title').a.get('href')
        title = i.find(class_='entry-title').text
        summary = i.find(class_='entry-summary').text
        result.append([_normalize_method_name(title),
                       link,
                       _normalize_description(summary) ])
    return result

def stdc_index(index_file):
    with open(index_file) as doc:
        soup = BeautifulSoup(doc.read(), 'lxml')
    
    return [ [i.get('name'), i.get('link')]
             for i in soup.find_all(['function', 'typedef', 'const', 'class', 'enum'])]

def stdcpp_index(index_file):
    with open(index_file) as doc:
        soup = BeautifulSoup(doc.read(), 'lxml')
    
    all_ = [ [i.get('name'), i.get('link')]
             for i in soup.find('index').find_all(
                     ['function', 'typedef', 'const'], recursive=False)
             if i.get('link') ]

    for block_item in soup.find('index').find_all(['class', 'enum'], recursive=False):
        block_name = block_item.get('name')
        block_link = block_item.get('link')
        all_.append([block_name, block_link])
        for i in block_item.find_all(['function', 'overload', 'const']):
            if i.get('link'):
                all_.append(["%s::%s" % (block_name, i.get('name')),
                             "%s/%s" % (block_link, i.get('link'))])
            else:
                all_.append(["%s::%s" % (block_name, i.get('name')),
                             block_link])
    return all_

def _print(f, *args):
    # TODO just ignore coding problems now
    safe_args = (i.strip().replace("\n", " ").encode('iso-8859-1', 'ignore') for i in args)
    print f.format(*safe_args)

def _sort_by_name(coll):
    return sorted(coll, key=lambda x: x[0])

def javadoc(uri):
    for name, link in _sort_by_name(javadoc_class_index(uri)):
        _print("{0}<->{1}", name, uri + link)

def javadoc_detail(uri):
    for name, link in _sort_by_name(javadoc_detail_index(uri)):
        # remove relative path ".."
        _print("{0}<->{1}", name, uri + link[3:])
    
def jdk6():
    javadoc("http://java.sun.com/javase/6/docs/api/")

def jdk6_detail():
    javadoc_detail("http://java.sun.com/javase/6/docs/api/")

def jee6():
    javadoc("http://java.sun.com/javaee/6/docs/api/")

def jdk7():
    javadoc("http://java.sun.com/javase/7/docs/api/")

def jdk7_detail():
    javadoc_detail("http://java.sun.com/javase/7/docs/api/")

def netty():
    javadoc("http://netty.io/4.0/api/")

def spring():
    javadoc("http://docs.spring.io/spring/docs/3.2.4.RELEASE/javadoc-api/")
    javadoc("http://docs.spring.io/spring-data/data-commons/docs/1.6.2.RELEASE/api/")

def jquery():
    uri = "http://api.jquery.com"
    for name, link, description in _sort_by_name(jquery_index(uri)):
        _print("{0}<->{1}<->{2}", name, description, link)

def stdc():
    index_file = "/home/xiao/opt/cppreference/index-functions-c.xml"
    link_base = "/home/xiao/opt/cppreference/reference/en.cppreference.com/w/"
    for name, link in _sort_by_name(stdc_index(index_file)):
        _print("{0}<->{1}", name, link_base + link + ".html")

def stdcpp():
    index_file = "/home/xiao/opt/cppreference/index-functions-cpp.xml"
    link_base = "/home/xiao/opt/cppreference/reference/en.cppreference.com/w/"
    for name, link in _sort_by_name(stdcpp_index(index_file)):
        if link is None: print name
        _print("{0}<->{1}", name, link_base + link + ".html")

def _trimFuncArgs(name):
    print name
    pos = name.find('(')
    if pos < 0 : return name 
    return name[:pos]

def sphinx_index(index_file):
    response = urlopen(index_file)
    soup = BeautifulSoup(response.read(), 'lxml')

    result = []
    for i in [ j
               for x in soup.find_all('td')
               for i in x.find_all('dl', recursive=False)
               for j in i.find_all('dt', recursive=False)]:
        name = _trimFuncArgs(i.a.text)
        result.append([name, i.a.get('href')])
    return result

def pymongo():
    uri = "http://api.mongodb.org/python/current/genindex.html"
    for name, link in sphinx_index(uri):
        _print("{0}<->{1}", name, "http://api.mongodb.org/python/current/" + link)

def usage():
    print "python anything-indexer.py jquery"
if __name__ == '__main__':
    if len(sys.argv) < 2:
        usage()
        exit(-1)
    command = sys.argv[1]
    if command == "jquery":
        jquery()
    if command == "jdk6":
        jdk6()
    if command == "jee6":
        jee6()
    if command == "jdk7":
        jdk7()
    if command == "jdk6-detail":
        jdk6_detail()
    if command == "jdk7-detail":
        jdk7_detail()
    if command == "netty":
        netty()
    if command == "stdc":
        stdc()
    if command == "stdcpp":
        stdcpp()
    if command == "spring":
        spring()
    if command == "pymongo":
        pymongo()

