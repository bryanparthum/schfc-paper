######################################
############################  preamble
######################################

## set the environment
using Pkg;
Pkg.activate(joinpath(@__DIR__, ".."));

## instantiate the environment
Pkg.instantiate();

## precompile
using Mimi, MimiGIVE, MimiRFFSPs, Random, CSV, DataFrames, DataDeps, Distributed, Statistics, Parquet;

## automatically get data dependancies (rffsps)
ENV["DATADEPS_ALWAYS_ACCEPT"] = "true"
MimiRFFSPs.datadep"rffsps_v5"

######################################
##################### model parameters
######################################

## set random seed for monte carlo 
seed = 42;

## set number of monte carlo draws
n = 10000;

## set emissions years
years = [2030];

## choose gas
gases = [:HFC23, :HFC32, :HFC43_10, :HFC125, :HFC134a, :HFC143a, :HFC227ea, :HFC245fa, :HFC152a, :HFC236fa, :HFC365mfc, :CO2]

## set named list of discount rates
discount_rates = 
    [
        (label = "2.0% Ramsey", prtp = exp(0.001972641)-1, eta  = 1.244458999),
    ];

## choose the model objects that you would like to save by uncommenting the lines (optional).
save_list = 
    [
    (:TempNorm_1850to1900, :global_temperature_norm),    # Global surface temperature anomaly (K) from preinudstrial
    (:total_forcing, :F_other_ghg),
    (:total_forcing, :scale_other_ghg)
    ];

######################################
###################### set up parallel
######################################

## add procs 
# addprocs(12);  ## 12 is the optimal number of processors for the given combination of HFCs and pulse years. However, we have made the default to be 1 processor to avoid accidentally overloading a user's machine.
addprocs(1);

## distribute packages
@everywhere using Pkg;
@everywhere Pkg.activate(joinpath(@__DIR__, ".."));
@everywhere using Mimi, MimiGIVE, Random;

######################################
####################### estimate scghg
######################################

pmap((year, gas) for 
    year in years, 
    gas in gases) do (year, gas)
        
    ## set random seed
    Random.seed!(seed);

    ## get model 
    m = MimiGIVE.get_model();

    ## specify meta analysis damages since we just need climate module outputs
    update_param!(m, :DamageAggregator, :include_ag, false)
    update_param!(m, :DamageAggregator, :include_cromar_mortality, false)
    update_param!(m, :DamageAggregator, :include_slr, false)
    update_param!(m, :DamageAggregator, :include_energy, false)
    update_param!(m, :DamageAggregator, :include_hs_damage, true)
    
    ## print iterations into console
    println("Now doing $gas in $year")

    ## specify output directory for save_list items
    output_dir = joinpath(@__DIR__, "../output/save_list/$gas-$year-n$n")

    results = 
        MimiGIVE.compute_scc(m, 
                            n                       = n , 
                            gas                     = gas, 
                            year                    = year, 
                            pulse_size              = 0.001,                  
                            discount_rates          = discount_rates, 
                            save_list               = save_list,               
                            output_dir              = output_dir);
 
end

## end of script, have a great day.
