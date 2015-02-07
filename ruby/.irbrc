require 'rubygems'
require 'irb/completion'
require 'irb/ext/save-history'

require 'pp'
require 'time'

vers = `rbenv version`.split(' ').first rescue nil
puts "Using \e[1m#{vers}\e[0m" if vers

IRB.conf[:AUTO_INDENT]  = true
IRB.conf[:PROMPT_MODE]  = :SIMPLE
IRB.conf[:HISTORY_FILE] = File.expand_path('~/.irb_history')
IRB.conf[:SAVE_HISTORY] = 1000

begin
  require 'wirble'

  Wirble.init
  Wirble.colorize

  colors = Wirble::Colorize.colors.merge({
   :object_class => :purple,
   :symbol => :purple,
   :symbol_prefix => :purple
  })
  Wirble::Colorize.colors = colors

rescue LoadError
  puts "Install the wirble gem for coloured output!"
end

require 'fileutils'
include FileUtils

def ls
  la.reject {|i| i[0] == '.' }
end

def la
  entries = Dir.entries(File.dirname(__FILE__))
  (entries - ["..", "."])
end

def pwd
  Dir.pwd
end

def cd(dir)
  Dir.chdir(dir)
  pwd
end

# http://ozmm.org/posts/time_in_irb.html
def time(times = 1)
  require 'benchmark'
  ret = nil
  Benchmark.bm { |x| x.report { times.times { ret = yield } } }
  ret
end
