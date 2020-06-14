ios =
{
  {
    name = 'Temperatura salon',
    data_type = 0, -- float
    type = 'temperature',
    service = 'io.1wire-sysfs',
    id = 34381576381,
    mode = 0 --input
  },
  {
    name = 'Temperatura pole',
    data_type = 0, -- float
    type = 'temperature',
    service = 'io.1wire-sysfs',
    id = 34381576877,
    mode = 0
  },
  {
    name = 'Kocioł grzanie',
    data_type = 1, -- integer
    type = 'binary_switch',
    service = 'io.gpio',
    id = 4,
    mode = 1
  },
  {
    name = 'Pompa cyrkulacji CWU',
    data_type = 1, -- integer
    type = 'binary_switch',
    service = 'io.gpio',
    id = 5,
    mode = 1 -- output
  }
}
weekly_schedules =
{
  {
    name = 'Timer tygodniowy',
    data_type = 1, -- integer
    triggers =
    {
      {
        day = 0, -- day of week, 0 = Sunday
        hour = 7, -- 24h format
        minute = 0,
        second = 0,
        value = 1
      },
      {
        day = 0,
        hour = 22,
        minute = 0,
        second = 0,
        value = 0
      },
      {
        day = 1,
        hour = 5,
        minute = 45,
        second = 0,
        value = 1
      },
      {
        day = 1,
        hour = 7,
        minute = 0,
        second = 0,
        value = 0
      },
      {
        day = 2,
        hour = 5,
        minute = 45,
        second = 0,
        value = 1
      },
      {
        day = 2,
        hour = 7,
        minute = 0,
        second = 0,
        value = 0
      },
      {
        day = 3,
        hour = 5,
        minute = 45,
        second = 0,
        value = 1
      },
      {
        day = 3,
        hour = 7,
        minute = 0,
        second = 0,
        value = 0
      },
      {
        day = 4,
        hour = 5,
        minute = 45,
        second = 0,
        value = 1
      },
      {
        day = 4,
        hour = 7,
        minute = 0,
        second = 0,
        value = 0
      },
      {
        day = 5,
        hour = 5,
        minute = 45,
        second = 0,
        value = 1
      },
      {
        day = 5,
        hour = 7,
        minute = 0,
        second = 0,
        value = 0
      },
      {
        day = 6,
        hour = 7,
        minute = 0,
        second = 0,
        value = 1
      },
      {
        day = 6,
        hour = 22,
        minute = 0,
        second = 0,
        value = 0
      }
    }
  }
}
interval_schedules =
{
  {
    name = 'Timer sekundowy',
    data_type = 1, -- integer
    interval = 1200000, -- in milliseconds
    values = 
    {
      3,
      200
    }
  },
  {
    name = 'Timer sekundowy float',
    data_type = 0, -- float
    interval = 1200000, -- in milliseconds
    values = 
    {
      3.1,
      2.9
    }
  }
}

rules =
{
  {
    name = 'Ogrzewanie',
    exec_func = 'heating_exec_func',
    triggers = {
      'Temperatura salon',
      'Temperatura pole',
      'Kocioł grzanie'
    }
  },
  {
    name = 'Cyrkulacja CWU',
    exec_func = 'circulation_exec_func',
    triggers = {
      'Timer tygodniowy',
      'Pompa cyrkulacji CWU'
    }
  },
  {
    name = 'Test rule',
    exec_func = 'test_exec_func',
    triggers = {
      'Timer sekundowy'
    }
  }
}

-- register_ios function is called at initialization to register known IO devices
function register_ios()
  id = 0
  for _, io in ipairs(ios) do
    register_io(
      id,
      io.name,
      io.data_type,
      io.type,
      io.service,
      io.id,
      io.mode)
    id = id + 1
  end

  for _, io in ipairs(interval_schedules) do
    register_interval_schedule(
      id,
      io.name,
      io.data_type,
      io.interval)
    for _, value in ipairs(io.values) do
      add_value_to_interval_schedule(
        id,
        value)
    end
    id = id + 1
  end

  for _, io in ipairs(weekly_schedules) do
    register_weekly_schedule(
      id,
      io.name,
      io.data_type)
    for _, trigger in ipairs(io.triggers) do
      add_trigger_to_weekly_schedule(
        id,
        trigger.day,
        trigger.hour,
        trigger.minute,
        trigger.second,
        trigger.value)
    end
    id = id + 1
  end
end

-- register_rules function is called at initialization to register rules
function register_rules()
  for rule_id, rule in ipairs(rules) do
    register_rule(rule_id, rule.name, rule.exec_func)
    for _, trigger in ipairs(rule.triggers) do
      add_trigger(rule_id, trigger)
    end
  end
end

function test_exec_func(rule_id, trigger_id)
  set_io_value(rule_id, 3, 1)
end