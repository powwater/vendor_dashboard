#!/bin/bash

gcloud secrets --project=powwater versions access latest --secret=config_powwater_vendorsdashboard --format='get(payload.data)' | tr '_-' '/+' | base64 -d > ../config.yml
