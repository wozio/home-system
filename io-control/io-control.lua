ios =
{
  {
    name = 'Temperatura salon',
    data_type = 0,
    type = 'temperature',
    data = {
      service = 'io.1wire-sysfs',
      id = 34381576381,
      mode = 0
    }
  },
  {
    name = 'Temperatura pole',
    data_type = 0,
    type = 'temperature',
    data = {
      service = 'io.1wire-sysfs',
      id = 34381576877,
      mode = 0
    }
  },
  {
    name = 'Kocioł grzanie',
    data_type = 1,
    type = 'binary_switch',
    data = {
      service = 'io.gpio',
      id = 4,
      mode = 1
    }
  },
  {
    name = 'Pompa cyrkulacji CWU',
    data_type = 1,
    type = 'binary_switch',
    data =
    {
      service = 'io.gpio',
      id = 5,
      mode = 1
    }
  },
  {
    name = 'Timer tygodniowy',
    type = 'weekly_schedule',
    data_type = 1,
    data =
    {
    }
  },
  {
    name = 'Timer sekundowy',
    type = 'interval_schedule',
    data_type = 1,
    data =
    {
      interval = 1000,
      value_1st = 1,
      value_1st = 0
    }
  }
}

rules =
{
  {
    name = 'Ogrzewanie',
    exec_func = 'heating_func',
    triggers = {
      1,
      2,
      3
    }
  },
  {
    name = 'Cyrkulacja CWU',
    exec_func = 'circulation_func',
    triggers = {
      4,
      5
    }
  },
  {
    name = 'Test rule',
    exec_func = 'test_func',
    triggers = {
      6
    }
  }
}

-- register_ios function is called at initialization to register known IO devices
function register_ios()
  for id, io in ipairs(ios) do
    register_io(id,
                io.name,
                io.data_type,
                io.type,
                io.data)
  end
end

-- register_rules function is called at initialization to register rules
function register_rules()
  for rule_id, rule in ipairs(rules) do
    register_rule(rule_id, rule.name, rule.exec_func)
    for _, trigger_id in ipairs(rule.triggers) do
      add_trigger(rule_id, trigger_id)
    end
  end
end
