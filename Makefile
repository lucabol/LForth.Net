all:
	dotnet build --nologo -v minimal

check:
	dotnet test --nologo -v minimal

run:
	dotnet run -v minimal --project Forth.Net.Cli/Forth.Net.Cli.csproj

pack:
	dotnet pack LForth.Net.Cli/LForth.Net.Cli.csproj --configuration Release --version-suffix alpha+$(shell date +%s)

packrelease:
	dotnet pack LForth.Net.Cli/LForth.Net.Cli.csproj --configuration Release

push: clean_pkg pack
	dotnet nuget push ./LForth.Net.Cli/nupkg/*.nupkg --source https://api.nuget.org/v3/index.json --api-key $(NUGET_FORTH)

pushrelease: clean_pkg packrelease
	dotnet nuget push ./LForth.Net.Cli/nupkg/*.nupkg --source https://api.nuget.org/v3/index.json --api-key $(NUGET_FORTH)

clean_pkg:
	trash -f ./LForth.Net.Cli/nupkg/*.nupkg || true
