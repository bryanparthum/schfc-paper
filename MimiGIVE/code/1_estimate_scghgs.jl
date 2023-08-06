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
years = [2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100];

## choose damage module
damages = :give;

## choose gas
gases = [:HFC23, :HFC32, :HFC43_10, :HFC125, :HFC134a, :HFC143a, :HFC227ea, :HFC245fa, :HFC152a, :HFC236fa, :HFC365mfc, :CO2]

## set named list of discount rates
discount_rates = 
    [
        (label = "2.0% Ramsey", prtp = exp(0.001972641)-1, eta = 1.244459),
        (label = "3.0% Ramsey", prtp = exp(0.007702710)-1, eta = 1.567899)
    ];

## GIVE results are in 2005 USD, this is the price deflator to bring the results to 2020 USD. accessed 10/19/2022. source: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2005&last_year=2020&scale=-99&categories=survey&thetable=
pricelevel_2005_to_2020 = 113.784/87.504;

######################################
###################### set up parallel
######################################

## add procs 
addprocs(54);

## distribute packages
@everywhere using Pkg;
@everywhere Pkg.activate(joinpath(@__DIR__, ".."));
@everywhere using Mimi, MimiGIVE, Random, CSV, DataFrames, Distributed, Statistics, Parquet;

######################################
###################### estimate scghgs
######################################

pmap((year, gas) for 
    year in years, 
    gas in gases) do (year, gas)
        
    ## set random seed
    Random.seed!(seed);

    ## get model 
    m = MimiGIVE.get_model();

    ## print iterations into console
    println("Now doing $gas for $damages damages in $year")

    results = 
        MimiGIVE.compute_scc(m, 
                            n                       = n , 
                            gas                     = gas, 
                            year                    = year, 
                            pulse_size              = 0.001,              ## scales the default pulse size of 1 kilotonne to 1 tonne 
                            certainty_equivalent    = true,                     
                            CIAM_GDPcap             = true, 
                            discount_rates          = discount_rates, 
                            save_cpc                = true)               ## must be true to recover certainty equivalent scghgs);
    
    ## blank data
    scghgs = DataFrame(sector = String[], discount_rate = String[], trial = Int[], scghg = Int[]);
        
    ## populate data
    for (k, v) in results[:scc]
        for (i, sc) in enumerate(v.ce_sccs)
            push!(scghgs, (sector = String(k.sector), discount_rate = k.dr_label, trial = i, scghg = round(Int, sc*pricelevel_2005_to_2020)))
        end
    end

    ## export full distribution    
    write_parquet(joinpath(@__DIR__, "../output/scghgs/full_distributions/sc-$gas-$damages-$year-n$n.parquet"), scghgs);

    ## collapse to the certainty equivalent scghgs
    scghgs_mean = combine(groupby(scghgs, [:sector, :discount_rate]), :scghg => (x -> round(Int, mean(x))) .=> :scghg);

    ## export average scghgs    
    write_parquet(joinpath(@__DIR__, "../output/scghgs/sc-$gas-$damages-$year.parquet"), scghgs_mean);

end

## end of script, have a great day.
