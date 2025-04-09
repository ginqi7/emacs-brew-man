# frozen_string_literal: true

require 'websocket_bridge'
require 'date'

class WebsocketBridgeDemo < WebsocketBridge::Base
  def initialize(app_name, server_port)
    super(app_name, server_port)
    @installed_cask_info_list = []
    @installed_formula_info_list = []
    @installed_tap_info_list = []
  end

  def query_tap_info(name)
    info = @installed_tap_info_list.find { |x| x.key?('name') && x['name'] == name }
    dictionary = {}
    dictionary[:name] = name
    if info
      dictionary[:formula] = info['formula_files'].length.to_s
      dictionary[:casks] = info['cask_files'].length.to_s
      dictionary[:name] = info['name']
      dictionary[:update] = info['last_commit']
    end
    dictionary
  end

  def query_cask_info(name)
    info = @installed_cask_info_list.find { |x| x.key?('token') && x['token'] == name }
    dictionary = {}
    dictionary[:name] = name
    if info
      dictionary[:desc] = info['desc'] if info['desc']
      dictionary[:tap] = info['tap']
      dictionary[:homepage] = info['homepage']
      dictionary[:version] = info['version']
      dictionary[:installedtime] =
        DateTime.strptime(info['installed_time'].to_s, '%s').strftime('%Y-%m-%d %H:%M:%S')
    end
    dictionary
  end

  def query_formula_info(name)
    info = @installed_formula_info_list.find { |x| x.key?('name') && x['name'] == name }
    dictionary = {}
    dictionary[:name] = name
    if info
      dictionary[:desc] = info['desc'] if info.key?('desc') && info['desc']
      dictionary[:tap] = info['tap']
      dictionary[:homepage] = info['homepage']
      dictionary[:version] = info['installed'][0]['version']
      dictionary[:installedtime] =
        DateTime.strptime(info['installed'][0]['time'].to_s, '%s').strftime('%Y-%m-%d %H:%M:%S')
    end
    dictionary
  end

  def run_to_json(cmd)
    JSON.parse(`#{cmd}`)
  end

  def on_message(data)
    case data[0]
    when 'refresh'
      @installed_tap_info_list = run_to_json('brew tap-info --json --installed')
      @installed_info_list = run_to_json('brew info --installed --json=v2')
      @installed_formula_info_list = @installed_info_list['formulae']
      @installed_cask_info_list = @installed_info_list['casks']
      run_in_emacs('message', 'brew-man data refreshed.')
    when 'tap-list'
      tap_list = `brew tap`.split("\n")
      func = data[1]
      run_in_emacs(func, *tap_list)
    when 'formula-list'
      tap_list = `brew list --formula`.split("\n")
      func = data[1]
      run_in_emacs(func, *tap_list)
    when 'cask-list'
      tap_list = `brew list --cask`.split("\n")
      func = data[1]
      run_in_emacs(func, *tap_list)
    when 'tap-info'
      tap_name = data[1]
      func = data[2]
      info = query_tap_info(tap_name)
      run_in_emacs(func, info)
    when 'cask-info'
      cask_name = data[1]
      func = data[2]
      info = query_cask_info(cask_name)
      run_in_emacs(func, info)
    when 'formula-info'
      formula_name = data[1]
      func = data[2]
      info = query_formula_info(formula_name)
      run_in_emacs(func, info)
    else
      puts "Unknown command #{data[0]}"
    end
  end
end

EM.run do
  WebsocketBridgeDemo.new(ARGV[0], ARGV[1])
end
