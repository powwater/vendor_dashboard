# MVP Requirements

> Powwater Vendor Dashboard Web Application MVP Requirements

## Screens (menus, tabsets, tabs, etc.)

1. Registration Screen
2. Login Screen
3. Landing Page
4. Order Detail Page
5. Notification Window
6. Rating Portal
7. Payments
8. Profile
9. Inventory
10. Mobile Money / Account Management



## Registration Screen

1. Initial Registration 
   - Forms handled through the [powpolished API](https://github.com/powwater/powpolished) and [dashboard](https://github.com/powwater/powwater,dashboard) for later use in authenticating users.
2. Build initial vendor profile
   - Similar to *Airbnb* profile with **location**, **type of water sold**, **bio**, **info on filtration plant**, **photos**, **info on water quality**, etc.
3. Upload documents
   - *Need more information*
4. Submission for approval
   - Final page to submit to Powwater for approval into program as a certified vendor.

***

- Registration Forms:
  - Username
  - Password
  - Primary Email
  - Phone Number
  - Bank Account Details - *MPesa*
- Build Profile:
  - Primary Fields:
    - Vendor Name / Business Name
    - Vendor Website and Contact Details
    - Vendor Operation Description and Biography
    - Vendor Location - selected via Google Maps API with Search Autocomplete - returns:
      - Location URL (google maps link)
      - Location Name
      - Location Address
      - Location Place ID
      - Location Coordinates (Latitude and Longitude)
    - Vendor *Region* - primary region in which vendor serves customers - should be a polygon place id
      - Region URL (google maps link)
      - Region Name
      - Region Place ID
    - Filtration Plant, Photos, Water Quality, etc.
- Upload Documents
- Submission for Approval
- Status Window

