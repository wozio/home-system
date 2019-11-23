io_temp_salon = 'Temperatura salon'
io_temp_zewn = 'Temperatura pole'
io_kociol = 'Kocioł grzanie'
io_cyrk = 'Pompa cyrkulacji CWU'
io_timer_tyg = 'Timer tygodniowy'

-- register_ios function is called at initialization to register known IO devices
function register_ios()
  print("LUA: register_ios")

  -- this is unique name of the io device, used to identify device in the system
  io_name = io_temp_salon
  io_type = 'temperature'
  io_data_type = 0 -- float data type
  io_service = 'io.1wire-sysfs'
  io_id = 34381576381
  io_mode = 0 -- this is input device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  io_name = io_temp_zewn
  io_type = 'temperature'
  io_data_type = 0 -- float data type
  io_service = 'io.1wire-sysfs'
  io_id = 34381576877
  io_mode = 0 -- this is input device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  io_name = io_kociol
  io_type = 'binary_switch'
  io_data_type = 1 -- integer data type
  io_service = 'io.gpio'
  io_id = 4
  io_mode = 1 -- this is output device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  io_name = io_cyrk
  io_type = 'binary_switch'
  io_data_type = 1 -- integer data type
  io_service = 'io.gpio'
  io_id = 5
  io_mode = 1 -- this is output device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  -- schedule is empty at the beginning, triggers are added below
  io_name = io_timer_tyg
  io_type = 'weekly_schedule'
  io_data_type = 1 -- integer data type
  register_io(io_name, io_data_type, io_type)

end

rule_ogrzewanie = 'ogrzewanie'
rule_cyrkulacja = 'cyrkulacja'

-- register_rules function is called at initialization to register rules
function register_rules()
  print('LUA: register_rules')

  register_rule(rule_ogrzewanie)
  add_trigger(rule_ogrzewanie, io_temp_salon)
  add_trigger(rule_ogrzewanie, io_temp_zewn)
  add_trigger(rule_ogrzewanie, io_kociol)

  register_rule(rule_cyrkulacja)
  add_trigger(rule_cyrkulacja, io_timer_tyg)
  add_trigger(rule_cyrkulacja, io_cyrk)
end
