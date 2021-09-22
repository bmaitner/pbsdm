# Plug-and-Play Species Distribution Modelling

## What is Plug-and-Play?
In general usage, the term plug-and-play refers to software or hardware that can be connected without any additional setup or configuration.  In the context of species distirbution models, plug-and-play is a framework developed by Drake and Richards (https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2373) that recognizes that specis distribution models can be constructed by "plugging in" any methods that can estimate density functions or the ratio of two density functions.


## The PBSDM R package
This repo contains a draft R package to implement PNP SDMs, with an emphasis on the presence-only and presence-background models (hence the current working name, PBSDM for presence-background species distribution models), as well as density-ratio models.  Most of the important functions in this package are wrappers around existing functions that handle density estimation or density-ratio estimation.  Much of this code was created by modifying existing code at https://github.com/DrakeLab/PlugNPlay in order to make functions more modular and extensible.
