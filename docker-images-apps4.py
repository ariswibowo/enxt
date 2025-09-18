import os
import time

images = [
    'elysiannxt/titanium-writer:11.0.1',
    'elysiannxt/ifrs9-measurement-result-writer:11.0.1',
    'elysiannxt/ifrs9-impairment-result-writer:11.0.2',
    'elysiannxt/historical-pd-result-writer:11.0.1',
    'elysiannxt/historical-lgd-result-writer:11.0.1',
    'elysiannxt/recovery-analysis-result-writer:11.0.1',
    'elysiannxt/titanium-sinker:11.1.1'
]

choice = input ("Please input the choice (pull/load/save)? ")

if choice.lower() in ("pull", "save", "load"):
    print(f"-- {choice.capitalize()} Docker Images --")
    print('Starting...')

    i = 0
    number = 1
    while i < len(images):
        # Set the images name
        if("empyreanapac" in images[i]):
            image_name = images[i].replace('empyreanapac/', '')
        elif("elysiannxt" in images[i]):
            image_name = images[i].replace('elysiannxt/', '')
        elif("portainer" in images[i]):
            image_name = images[i].replace('portainer/', '')
        elif("wurstmeister" in images[i]):
            image_name = images[i].replace('wurstmeister/', '')
        elif("mcr.microsoft.com" in images[i]):
            image_name = images[i].replace('mcr.microsoft.com/dotnet/', '')

        image_name = image_name.replace(':', '_')

        # Logging to console
        print(f'{number} - Processing {images[i]} ⏳')

        # Running the command
        if choice.lower() == "save":
            run = os.system(f'docker image save -o  {image_name}.tar {images[i]}')
        elif choice.lower() == "pull":
            run = os.system(f'docker pull {images[i]}')
        elif choice.lower() == "load":
            run = os.system(f'docker load -i {image_name}.tar')

        i += 1
        number += 1

    print('✅ Finished.')
else:
    print('⚠️ Invalid choice, please enter pull, save, or load')