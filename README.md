![petr](https://github.com/icssc-projects/peterportal-public-api/blob/master/public/images/peterportal-banner-logo.png?raw=true)

PeterPortal is a web application aimed to aid UCI students with course discovery. We consolidate public data available on multiple UCI sources on the application to improve the user experience when planning their course schedule.

🔨 Built with:

* PeterPortal API
* ExpressJS
* ReactJS
* SST and AWS CDK
* MongoDB
* GraphQL
* Typescript

## First time setup
1. Clone the repository to your local machine:
    ```
    git clone https://github.com/icssc/peterportal-client
    ```

2. Switch to a branch you will be working on.
    ```
    git checkout -b [branch name]
    ```

3. Check your Node version with `node -v`. Make sure you have version 14 or above (18 recommended).

4. Open a terminal window and `cd` into the directory of your repo.

5. Run `npm install` to install all node dependencies for the site and API. This may take a few minutes.

## Running the project locally (after setup)
1. Open two terminal windows and `cd` into the directory of your repo in each of them.

2. In the first terminal window, enter the client directory with `cd site`. Then run the React development server using `npm start`. Ensure the server is running on port 3000 by default.

3. In the second terminal window, enter the API directory with `cd api`. Then run the Express development server using `npm run dev`. Ensure the server is running on port 5000 by default.

## Our Mission
🎇 Our mission is to improve the UCI student experience with course planning

## Where does the data come from?

We consolidate our data directly from official UCI sources such as: UCI Catalogue, UCI Public Records Office, and UCI Webreg (courtesy of [PeterPortal API](https://github.com/icssc/peterportal-api-nex)).

## Bug Report
🐞 If you encountered any issues or bug, please open an issue @ https://github.com/icssc-projects/peterportal-client/issues/new


## Other Disclaimer
✅ Although we consolidate our data directly from official UCI sources, this application is by all means, not an official UCI tool. We stride to keep our data as accurate as possible with the limited support we have from UCI. Please take that into consideration while using this Website.

## Terms & Conditions
📜 There are no hard policies at the moment for utilizing this tool. However, please refrain from abusing the Website by methods such as: sending excessive amount of requests in a small period of time or purposely looking to exploit the system.
