# frozen_string_literal: true

require 'websocket_bridge'

class WebsocketBridgeDemo < WebsocketBridge::Base
  def parse_tap_info(info)
    dictionary = {}
    info.split("\n").each do |line|
      dictionary[:formula] = line.gsub(' formula', '') if line.end_with?('formula')
      dictionary[:update] = line.gsub('last commit: ', '') if line.start_with?('last commit: ')
      dictionary[:casks] = line.gsub(' casks', '') if line.end_with?('casks')
      dictionary[:name] = line.gsub(': Installed', '') if line.end_with?(': Installed')
    end
    dictionary
  end

  def on_message(data)
    case data[0]
    when 'tap-list'
      tap_list = `brew tap`.split("\n")
      func = data[1]
      run_in_emacs(func, *tap_list)
    when 'tap-info'
      tap_name = data[1]
      func = data[2]
      puts tap_name
      puts func
      info = `brew tap-info #{tap_name}`
      dic = parse_tap_info(info)
      run_in_emacs(func, dic)
    when 'orange'
      puts 'You selected an orange!'
    else
      puts 'Unknown fruit!'
    end
  end
end

EM.run do
  WebsocketBridgeDemo.new(ARGV[0], ARGV[1])
end
