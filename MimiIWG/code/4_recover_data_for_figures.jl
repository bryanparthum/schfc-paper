######################################
############################  preamble
######################################

## set the environment
using Pkg;
Pkg.activate(joinpath(@__DIR__, ".."));

## instantiate the environment
Pkg.instantiate();

### precompile and add packages to the namespace
using Random, Mimi, MimiIWG, Distributed;

######################################
##################### model parameters
######################################

## set random seed for monte carlo 
seed = 42;

## set number of monte carlo draws
n = 10000;

## set emissions year
years = [2030]

## select models 
models = [:dice, :fund, :page];

## choose gases
gases = [:HFC23, :HFC32, :HFC125, :HFC134a, :HFC143a, :HFC152a, :HFC227ea, :HFC236fa, :HFC245fa, :HFC365mfc, :HFC4310mee, :CO2];

## set list of discount rates
discount_rates = [0.03];

######################################
###################### set up parallel
######################################

## add procs 
addprocs(36);

## distribute packages
@everywhere using Pkg;
@everywhere Pkg.activate(joinpath(@__DIR__, ".."));
@everywhere using Random, Mimi, MimiIWG, Distributed;

######################################
########################## recover mcs
######################################

pmap((model, gas) for 
    model in models, 
    gas in gases) do (model, gas)

    ## print iterations into console
    println("Now doing $model for $gas")

    if Symbol("$model") == :dice

        ##  DICE
        Random.seed!(seed)
        MimiIWG.run_scc_mcs(DICE,
            gas                = Symbol("$gas"), 
            trials             = n,
            perturbation_years = years,
            discount_rates     = discount_rates,
            tables             = false,
            output_dir         = joinpath(@__DIR__, "..", "output/save_list/$gas/dice"),
            save_list          = [(:climatedynamics, :TATM)],
            save_scc           = false);

    elseif Symbol("$model") == :page

        ##  PAGE
        Random.seed!(seed)
        MimiIWG.run_scc_mcs(PAGE, 
            gas                = Symbol("$gas"), 
            trials             = n,
            perturbation_years = years,
            discount_rates     = discount_rates,
            domestic           = false,
            tables             = false,
            output_dir         = joinpath(@__DIR__, "..", "output/save_list/$gas/page"),
            save_list          = [(:ClimateTemperature, :rt_g_globaltemperature)],
            save_scc           = false);

    elseif Symbol("$model") == :fund

        ##  FUND
        Random.seed!(seed)
        MimiIWG.run_scc_mcs(FUND, 
            gas                = Symbol("$gas"), 
            trials             = n,
            perturbation_years = years,
            discount_rates     = discount_rates,
            domestic           = false,
            tables             = false,
            output_dir         = joinpath(@__DIR__, "..", "output/save_list/$gas/fund"),
            save_list          = [(:climatedynamics, :temp)],
            save_scc           = false);

    end

end

## end of script, have a great day.