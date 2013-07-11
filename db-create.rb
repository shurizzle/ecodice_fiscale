#!/usr/bin/env ruby

require 'net/http'
require 'fileutils'

class Saver
  def initialize(dir = './src')
    @locations = []
    @dir = File.realpath(dir)
    @file = File.join(@dir, 'belfiore.erl')
    FileUtils.touch(@file)
    @fp = File.open(@file, 'a')
    @fp.puts('-module(belfiore).')
    @fp.puts
    @fp.puts('-compile({no_auto_import, [get/1]}).')
    @fp.puts('-export([get/2]).')
    @fp.puts
  end

  def save(code, city, province = nil)
    return if code !~ /^[a-z]\d{3}$/i or city.empty?
    province ||= 'EE'
    city = (city.upcase rescue city).to_s
    province = (province.upcase rescue province).to_s

    unless @locations.include?([city, province])
      @locations << [city, province]
      @fp.puts "get(\"#{city}\", \"#{province}\") -> \"#{code.upcase rescue code}\";"
    end
  end

  def close
    t = @fp.tell - 2
    @fp.close
    File.truncate(@file, t)
    @fp = File.open(@file, 'a')
    @fp.puts('.')
    @fp.close
  end
end

class Browser
  def self.get_page(url)
    Net::HTTP.get(URI.parse(url))
  end

  def self.inherited(obj)
    @@objects ||= []
    @@objects << obj
  end

  def self.objects
    @@objects ||= []
  end

  def self.download_all
    saver ||= Saver.new(File.join(File.dirname(__FILE__), 'src'))

    self.objects.each {|o|
      o.download(saver) if o.respond_to?(:download)
    }

    saver.close
  end
end

class Cities < Browser
  PAGE_ROOT = 'http://www.dossier.net/guida/codicicatastali/'
  PAGES = ['index', ?b, ?c, ?d, ?e, ?f, 'g-h', 'i-j', ?l, ?m, ?n, ?o, ?p, ?q, ?r, ?s, ?t, ?u, ?v, ?z].map {|s| "#{PAGE_ROOT}#{s}.html" }

  def self.download(saver)
    self::PAGES.each {|page|
      self.get_page(page).scan(/^([A-Z][0-9]{3})\s{3}(.+?)\s+([A-Z]{2})/m).each {|g|
        saver.save(*g)
      }
    }
  end
end

class EE < Browser
  PAGE = 'http://www.demografici.it/servizi/notizie/notizia_v.asp?ID=3'

  def self.download(saver)
    self.get_page(self::PAGE).match(/<table class="Tabella">(.+?)<\/table>/m)[1].scan(/<tr>(.+?)<\/tr>/m).flatten.each {|tr|
      zona, belf = tr.scan(/<td.+?>(.+?)<\/td>\s+<td>.*?<\/td>\s+<td>.*?<\/td>\s+<td>.*?<\/td>\s+<td>.*?<\/td>\s+<td>(.+?)<\/td>/m).flatten.map {|e|
        e.strip.gsub(/<.+?>|<\/.+?>/, '')
      }
      zona = zona.gsub(/\s*\(.+?\).*?$/, '').gsub(/^.*?=/, '').strip

      saver.save(belf, zona)
      saver.save(belf, 'GUAYANA') if zona == 'GUIANA'
    }
  end
end

Browser.download_all
