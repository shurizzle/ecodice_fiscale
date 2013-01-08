all:
	@rebar compile

clean:
	@rebar clean

generate:
	-@rm -f src/belfiore.erl
	@ruby db-create.rb
