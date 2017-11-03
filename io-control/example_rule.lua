-- exec function is called every time one of the connected io devices changes its value or state
function exec()
    -- this is name of the io device, must be exactly the same as in configuration file
    n = 'timer swiatla na zewnatrz'
    -- getting io device state and value
    s,v = get_io_state_value(n)
    if s == 0 then
        print('state is unknown')
    elseif s == 1 then
        print('state is OK')
        -- only when state is OK value has valid... value
        print(v)
    else
        print('state is failed')
    end
    -- globals are preserved on multiple runs
    f = f + v
    print(f)
end
print('example rule script loaded! YAY!')
f = 122