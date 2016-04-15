cowboy_enhancer
=====

An OTP application

Build
-----

    $ rebar3 compile

Easy to setup
----
Just create an application with rebar3

<pre>rebar3 new release myapp</pre>

in the config/sys.config file put this:
<pre>
[
    {lager, [
        {handlers, [
            {lager_console_backend, [info, {lager_default_formatter,
                ["  ==> ", time, " [", severity, "]: ", message, "\n"]}]},
            {lager_file_backend, [
                {file, "log/error.log"}, {level, error}, {formatter, lager_default_formatter},
                {formatter_config, [
                    date, " ", time, " [", severity, "] ", pid, " ", message, "\n"]}]},
            {lager_file_backend, [
                {file, "log/console.log"}, {level, info}, {formatter, lager_default_formatter},
                {formatter_config, [
                    date, " ", time, " [", severity, "] ", pid, " ", message, "\n"]}]}
        ]},
        {error_logger_hwm, 222250}
    ]},
    {cowboy_enhancer, [
        %% This is the main application name.
        {target_app, myapp},

        {templates_dir, "/view/templates"},

        %% shows more info in the console when system starts,
        %% more bigger the number is more detailed info is shown.
        %% 0 -> no info, 3 -> maximum details.
        {system_start_verbose_level, 2},

        %% Database backends configuration.
        {database_manager, [
            {main_backend, [
                {backend, postgres_backend},
                {server, "localhost"},
                {username, "postgres"},
                {password, "server"},
                {database, "eoc_db"},
                %% max amount of database connections in the connection pool.
                {max_reusable_connections, 10}, % 10 connections.
                %% max time to wait for an available connection.
                {wait_for_reusable_connection_timeout, 10000} % 10 seconds.
            ]}
        ]},

        %% Configuration for session manager module.
        {session_manager, [
            %% this is the default time in which sessions will expire.
            {session_expire_time, 36000000}, % 1 hour.
            %% this is the default frequency in which expired sessions will be
            %% recollected and deleted.
            {garbage_collector_frequency, 3600000} % 1 hour.
        ]}
    ]}
].
</pre>

Configure it at will.

Then put the cowvoy_enhancer dependency in rebar.config file located in myapp folder,

<pre>
{deps, [
    {cowboy_enhancer, {git, "git://github.com/StartSWest/cowboy_enhancer.git", {branch, master}}}]}.
</pre>

to start the system in development mode with your application included:

<pre>rebar3 shell</pre>