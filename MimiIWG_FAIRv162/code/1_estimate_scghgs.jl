## prior to running this script, the registry needs to be added to your machine. this can be done at the command line
## by starting Julia, then typing "]" (without the quotes) to get to the package command. 
## Then, type "registry add https://github.com/mimiframework/MimiRegistry.git" (without the quotes) to connect your machine
## to the Mimi registry. Then, proceed with running this script either in the command line or other (see README). 

######################################
############################  preamble
######################################

## automatically get data dependancies "mimiiwg_fairv162_paths"
ENV["DATADEPS_ALWAYS_ACCEPT"] = "true"
MimiIWG_FAIRv162.datadep"mimiiwg_fairv162_temp_trajectories"

## set the environment
using Pkg;
Pkg.activate(joinpath(@__DIR__, ".."));

## instantiate the environment
Pkg.instantiate();

## precompile and add packages to the namespace
using Random, Mimi, MimiIWG_FAIRv162, Distributed

## set random seed for monte carlo 
seed = 42;

## set number of monte carlo draws
n = 10000

## set emissions year
years = collect(2020:10:2100);
years_page = collect(2020:10:2080);

## select models 
models = [:dice, :fund, :page];

## choose gases
gases = [:HFC23, :HFC32, :HFC43_10, :HFC125, :HFC134a, :HFC143a, :HFC227ea, :HFC245fa, :HFC152a, :HFC236fa, :HFC365mfc, :CO2];

## set list of discount rates
discount_rates = [0.03];

######################################
###################### set up parallel
######################################

## add procs 
addprocs(55);

## distribute packages
@everywhere using Pkg;
@everywhere Pkg.activate(joinpath(@__DIR__, ".."));
@everywhere using Random, Mimi, Distributed, MimiIWG_FAIRv162

######################################
####################### estimate scghg
######################################

pmap((model, gas) for 
    model in models, 
    gas in gases) do (model, gas)

    ## print iterations into console
    println("Now doing $model for $gas")

    if Symbol("$model") == :dice

    ##  DICE
    Random.seed!(seed)
    MimiIWG_FAIRv162.run_scc_mcs(DICE,
        gas                = Symbol("$gas"), 
        trials             = n,
        perturbation_years = years,
        discount_rates     = discount_rates,
        domestic           = false,
        tables             = false,
        output_dir         = joinpath(@__DIR__, "..", "output/scghgs/$gas/dice"));
        
    elseif Symbol("$model") == :page

        ##  PAGE
        Random.seed!(seed)
        MimiIWG_FAIRv162.run_scc_mcs(PAGE, 
            gas                = Symbol("$gas"), 
            trials             = n,
            perturbation_years = years_page,
            discount_rates     = discount_rates,
            domestic           = false,
            tables             = false,
            output_dir        = joinpath(@__DIR__, "..", "output/scghgs/$gas/page"));

    elseif Symbol("$model") == :fund

        ##  FUND
        Random.seed!(seed)
        MimiIWG_FAIRv162.run_scc_mcs(FUND, 
            gas                = Symbol("$gas"), 
            trials             = n,
            perturbation_years = years,
            discount_rates     = discount_rates,
            domestic           = false,
            tables             = false,
            output_dir         = joinpath(@__DIR__, "..", "output/scghgs/$gas/fund"));
    
    end

end

## end of script, have a great day.
