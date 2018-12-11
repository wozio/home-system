

-- register_ios function is called at initialization to register known IO devices
function register_ios()
  print("LUA: register_ios")

  -- this is unique name of the io device, used to identify device in the system
  io_name = 'Temperatura salon'
  io_type = 'temperature'
  io_data_type = 0 -- float data type
  io_service = 'io.1wire-sysfs'
  io_id = 34381576381
  io_mode = 0 -- this is input device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  io_name = 'Temperatura pole'
  io_type = 'temperature'
  io_data_type = 0 -- float data type
  io_service = 'io.1wire-sysfs'
  io_id = 34381576877
  io_mode = 0 -- this is input device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  io_name = 'Kocioł grzanie'
  io_type = 'binary_switch'
  io_data_type = 1 -- integer data type
  io_service = 'io.gpio'
  io_id = 4
  io_mode = 1 -- this is output device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  io_name = 'Pompa cyrkulacji CWU'
  io_type = 'binary_switch'
  io_data_type = 1 -- integer data type
  io_service = 'io.gpio'
  io_id = 5
  io_mode = 1 -- this is output device
  register_io(io_name, io_data_type, io_type, io_service, io_id, io_mode)

  -- schedule is empty at the beginning, triggers are added below
  io_name = 'Timer tygodniowy'
  io_type = 'weekly_schedule'
  io_data_type = 1 -- integer data type
  register_io(io_name, io_data_type, io_type)

end

-- register_rules function is called at initialization to register rules
function register_rules()
  print('LUA: register_rules')
end
