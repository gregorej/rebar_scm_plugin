-module(rebar_scm_plugin).

-export([
	pre_scm/2,
	scm/2
]).

-include_lib("rebar/include/rebar.hrl").


%%
% url() ::= {protocol, [Path]}


as_url(UrlString) ->
	Sep = "://",
	L = length(Sep),
	case string:str(UrlString, Sep) of
		0 -> {error, {invalid_url, UrlString}};
		Index ->
			ProtocolStr = lists:sublist(UrlString, 1, Index - 1),
			PathStr = lists:sublist(UrlString, Index + L, length(UrlString) - Index - L + 1),
			{ok, {list_to_atom(ProtocolStr), string:tokens(PathStr, "/")}}
	end.


as_string({Protocol, PathParts})  ->
	atom_to_list(Protocol) ++ "://" ++ string:join(PathParts, "/").


pre_scm(Config, AppFile) ->
	rebar_config:set_global(skip_deps, true),
	ok.

% copied from http://hyperthunk.github.com/rebar-plugin-tutorial/part-2-plugin-anatomy/index.html
is_base_dir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).

scm(Config, AppFile) ->
	?DEBUG("Works!~nConfig: ~p~nAppFile: ~p~n", [Config, AppFile]),
	case is_base_dir() of
		true -> 
			Tag = rebar_config:get_global(tag, undefined),
			Scm = rebar_config:get_local(Config, scm, undefined),
			case Scm of
				undefined -> 
					?ERROR("No SCM information provided in rebar.config file~n", []),
					?FAIL;
				_ -> ok
			end,
			?DEBUG("Tag = ~p~n", [Tag]),
			case Tag of
				undefined -> 
					?ERROR("No tag specified", []),
					?FAIL;
				_ -> 
					?INFO("Tagging with tag ~s~n", [Tag]),
					tag(Scm,Tag)
			end;
		false -> 
			?DEBUG("Ignoring dependency ",[])
	end,
	ok.

tag({svn, UrlStr}, Tag) when is_list(UrlStr) ->
	{ok, {Protocol, Path}} = as_url(UrlStr),
	TargetPath = case lists:last(Path) of
		"trunk" -> lists:append(lists:sublist(Path, length(Path) - 1), ["tags",Tag]);
		_ -> lists:append(Path, ["tags", Tag])
	end,
	rebar_utils:sh("svn copy " ++ UrlStr ++ " " ++ as_string({Protocol, TargetPath}), []);

tag({git, _Url}, Tag) ->
	?Tag("Tagging with Git ~s", [Url]),
	rebar_utils:sh("git tag " ++ Tag).


tag({Scm, _},Tag) ->
	?ERROR("Unknown scm: ~p", [Scm]).
