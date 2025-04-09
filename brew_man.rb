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

  def tap_count(lst)
    lst.group_by { |item| item['tap'] }.transform_values(&:count)
  end

  def tap_info_list
    formula_tap_count = tap_count(@installed_formula_info_list)
    cask_tap_count = tap_count(@installed_cask_info_list)
    @installed_tap_info_list.map do |info|
      dictionary = {}
      totoal_formulae = info['formula_names'].length.to_s
      totoal_casks = info['cask_tokens'].length.to_s
      dictionary[:name] = info['name']
      dictionary[:update] = info['last_commit'] if info['last_commit']
      installed_formulae = 0
      if formula_tap_count.key?(info['name']) && formula_tap_count[info['name']]
        installed_formulae = formula_tap_count[info['name']]
      end
      dictionary[:formulae] =
        "#{installed_formulae} / #{totoal_formulae}"
      installed_casks = 0
      if cask_tap_count.key?(info['name']) && cask_tap_count[info['name']]
        installed_casks = cask_tap_count[info['name']]
      end
      dictionary[:casks] =
        "#{installed_casks} / #{totoal_casks}"

      dictionary
    end
  end

  def base_convert(info)
    dictionary = {}
    dictionary[:desc] = info['desc'] if info.key?('desc') && info['desc']
    dictionary[:tap] = info['tap'] if info.key?('tap') && info['tap']
    dictionary[:homepage] = info['homepage'] if info.key?('homepage') && info['homepage']
    dictionary
  end

  def info_list
    formulae = @installed_formula_info_list.map do |info|
      dictionary = base_convert(info)
      dictionary[:type] = 'formula'
      dictionary[:name] = info['name']
      dictionary[:version] = info['installed'][0]['version']
      dictionary[:installedtime] =
        DateTime.strptime(info['installed'][0]['time'].to_s, '%s').strftime('%Y-%m-%d %H:%M:%S')
      dictionary
    end

    casks = @installed_cask_info_list.map do |info|
      dictionary = base_convert(info)
      dictionary[:type] = 'cask'
      dictionary[:name] = info['name'][0]
      dictionary[:version] = info['version'] if info.key?('version') && info['version']
      dictionary[:installedtime] =
        DateTime.strptime(info['installed_time'].to_s,
                          '%s').strftime('%Y-%m-%d %H:%M:%S')
      dictionary
    end
    formulae + casks
  end

  def run_to_json(cmd)
    JSON.parse(`#{cmd}`)
  end

  def refresh_data
    @installed_tap_info_list = run_to_json('brew tap-info --json --installed')
    @installed_info_list = run_to_json('brew info --installed --json=v2')
    @installed_formula_info_list = @installed_info_list['formulae']
    @installed_cask_info_list = @installed_info_list['casks']
    run_in_emacs('message', 'brew-man data refreshed.')
  end

  def on_message(data)
    case data[0]
    when 'refresh'
      refresh_data
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
    when 'tap-list'
      tap_list = tap_info_list
      func = data[1]
      run_in_emacs(func, tap_list)
    when 'list'
      tap_list = info_list
      func = data[1]
      run_in_emacs(func, tap_list)
    when 'run-command'
      cmd = data[1]
      result = `#{cmd}`
      func = data[2]
      run_in_emacs(func, result)
      run_in_emacs('message', "Command [#{cmd}] Success.")
    else
      puts "Unknown command #{data[0]}"
    end
  end
end

EM.run do
  WebsocketBridgeDemo.new(ARGV[0], ARGV[1])
end
