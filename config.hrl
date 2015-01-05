-define(SOCK_OPTS, [binary, {active, true}
                            % {sndbuf, 11000},
                            % {recbuf, 11000}
                   ]).
-define(PORT, 1337).
-define(HOST, "localhost").
-define(TIMEOUT, 1000).
-define(PACKET_SIZE, 6000).
