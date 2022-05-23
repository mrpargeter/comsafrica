#### Range histograms-arranged by IRR performance (worst to best) ####

# platfthickmid
ggplot(data=filter(flake_measurements_summary,variable=="platfthickmid"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Platform thickness mid-point",
           x ="Range of measurements", y = "Density")

# techwidthdist
ggplot(data=filter(flake_measurements_summary,variable=="techwidthdist"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological width distal",
           x ="Range of measurements", y = "Density")

# techthickprox
ggplot(data=filter(flake_measurements_summary,variable=="techthickprox"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological thickness proximal",
           x ="Range of measurements", y = "Density")

# techlength
ggplot(data=filter(flake_measurements_summary,variable=="techlength"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological length",
           x ="Range of measurements", y = "Density")

# platfthickmax
ggplot(data=filter(flake_measurements_summary,variable=="platfthickmax"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Platform thickness maximum",
           x ="Range of measurements", y = "Density")

# platfwidth
ggplot(data=filter(flake_measurements_summary,variable=="platfwidth"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Platform width",
           x ="Range of measurements", y = "Density")

# platfthickimpact
ggplot(data=filter(flake_measurements_summary,variable=="platfthickimpact"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Platform thickness impact",
           x ="Range of measurements", y = "Density")

# techwidthprox
ggplot(data=filter(flake_measurements_summary,variable=="techwidthprox"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological width proximal",
           x ="Range of measurements", y = "Density")

# techmaxthickness
ggplot(data=filter(flake_measurements_summary,variable=="techmaxthickness"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological max thickness",
           x ="Range of measurements", y = "Density")

# techmaxwidth
ggplot(data=filter(flake_measurements_summary,variable=="techmaxwidth"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological max width",
           x ="Range of measurements", y = "Density")

# techthickdist
ggplot(data=filter(flake_measurements_summary,variable=="techthickdist"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological thickness distal",
           x ="Range of measurements", y = "Density")

# techwidthmes
ggplot(data=filter(flake_measurements_summary,variable=="techwidthmes"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological width mesial",
           x ="Range of measurements", y = "Density")

# techthickmes
ggplot(data=filter(flake_measurements_summary,variable=="techthickmes"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Technological thickness mesial",
           x ="Range of measurements", y = "Density")

# maximumthickness
ggplot(data=filter(flake_measurements_summary,variable=="maximumthickness"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Maximum thickness",
           x ="Range of measurements", y = "Density")

# dorsal_cortex COME BACK TO THIS AFTER CHECKING DATA FOR WEIRD RANGE
# VALUES
ggplot(data=filter(flake_measurements_summary,variable=="dorsal_cortex"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Dorsal cortex",
           x ="Range of measurements", y = "Density")

# maximumwidth
ggplot(data=filter(flake_measurements_summary,variable=="maximumwidth"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Maximum width",
           x ="Range of measurements", y = "Density")

# Max dimension
ggplot(data=filter(flake_measurements_summary,variable=="maximumdimension"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Maximum dimension",
           x ="Range of measurements", y = "Density")

# mass
ggplot(data=filter(flake_measurements_summary,variable=="mass"), aes(x=range)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      labs(title="Mass",
           x ="Range of measurements", y = "Density")
