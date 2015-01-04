-define(SOCK_OPTS, [binary, {active, false},
                            {sndbuf,4194304},
                            {recbuf,4194304},
                            {nodelay, true},
                            {packet, raw}]).
-define(PORT, 1337).
-define(HOST, "localhost").
-define(TIMEOUT, 30000).
-define(PACKET_SIZE, 60000).
