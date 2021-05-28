# pRSL: Transfer Learning Application for Human Activity Recognition in Logistics

This repo contains the main code to run the transfer learning experiments described in the ICCL 2021 submission. The main folders give the corresponding reference dataset. Each subfolder corresponds to a transfer experiment, i.e. manual rules or automatic rules, and some additions. Each of these experiments is self-contained, that is it includes a (possibly outdated) rsl.R and norn.R backend, an experiment.R file that runs the hyperparameter tuning and creates *_res.RData files, an experimentFinal.R file for the chosen final hyperparameter, and an analysis.R file that summarizes the results. The required tCNN code is omitted for copyright reasons, message me if you need it. The preprocessed LARa, RealDisp and AndyLab data is hosted externally due to size constraints on  https://tu-dortmund.sciebo.de/s/yJO63mrr2jry5BC . Feel free to send me a message if there are problems in downloading. 

To reproduce the experiments:
1. Select a dataset folder
2. Run the hyperparameter tuning experiment.R
3. Select hyperparameters by running the first part of analysis.R
4. Run experimentFinal.R after possibly adjusting its hyperparameters
5. Summarize the results via  the second part of analysis.R

# Further Resources:

- Main pRSL code repo (with up-to-date version of pRSL): https://github.com/mkirchhof/rsl
- Approximate inference experiments: https://github.com/mkirchhof/rslSim
- Benchmarks of pRSL on multi-label datasets: https://github.com/mkirchhof/rslBench
- Application of pRSL for transfer learning in human activity recognition: https://github.com/mkirchhof/rslAppl
- Paper on pRSL: Kirchhof, M., Schmid, L., Reining, C., ten Hompel, M., Pauly, M.: pRSL: Interpretable Multi–label Stacking by Learning Probabilistic Rules. In: Proceedings of the 37th Conference on Uncertainty in Artificial Intelligence, PMLR, in press (2021).