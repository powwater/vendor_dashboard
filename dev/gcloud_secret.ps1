$project = "powwater"
$secret_name = "config_powwater_vendorsdashboard"
$out_file = "config.yml"

$secret = gcloud secrets --project=${project} versions access latest --secret=${secret_name} --format='get(payload.data)'

$out = $secret | wsl -e tr '_-' '/+' | wsl -e base64 -d

$out >> $out_file
