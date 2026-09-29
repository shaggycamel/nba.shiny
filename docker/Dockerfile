FROM nba.shiny_base:latest

# Copy app source
COPY nba.shiny_*.tar.gz .
RUN R -e 'remotes::install_local("nba.shiny_*.tar.gz", upgrade="never")' && rm nba.shiny_*.tar.gz

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
    CMD curl -f http://localhost:3838/ || exit 1

EXPOSE 3838
USER rstudio
CMD R -e "options(shiny.port=3838, shiny.host='0.0.0.0'); nba.shiny::run_app()"