Rebar SCM plugin
====================

Configuration
-------------------------------

In order to use this plugin you have to have following information in your ```rebar.config``` file:

```erlang
{plugins, [rebar_scm_plugin]}
```

```erlang
{scm, {Protocol, "Path to your scm root"}}
```
where ```Protocol``` is one of following:

* ```svn```
* ```git```

```erlang
{deps, {git, "git@github.com:gregorej/rebar_scm_plugin"}}
```


Usage
--------------------------------

Tagging version:

```sh
rebar tag=YourTag scm
```


