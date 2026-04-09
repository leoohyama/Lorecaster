# 1. Use the version-stable R 4.5.2 image (comes with shiny and tidyverse)
FROM --platform=linux/amd64 rocker/shiny-verse:4.5.2

# 2. Install ONLY the system dependencies for RPostgres and Plotly
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# 3. Set the working directory
WORKDIR /home/lorecaster

# 4. Copy renv files first (this optimizes the build cache)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# 5. Restore the exact library versions from your lockfile
RUN R -e "renv::restore()"

# 6. Copy the Lorecaster dashboard code
COPY . .

# 7. Open the Shiny port
EXPOSE 3838

# 8. Command to launch your app
CMD ["R", "-e", "shiny::runApp('shiny_app', host = '0.0.0.0', port = 3838)"]