# AWS Configuration
- Alternative Source: https://itnext.io/run-your-containers-on-aws-fargate-c2d4f6a47fda

# Deploy to AWS Fargate  
1. Go to https://us-west-1.console.aws.amazon.com/ecs/home?region=us-west-1#/firstRun
2. Configure custom container definition
- ![Alt text](config_1.png)
    - Container name: peterportal
    - Image: registry.hub.docker.com/peterportal/peterportal (link to dockerhub image)
    - Memory Limits: 256 
    - Port mappings: 5000 (port specified on Express server)
- ![Alt text](config_2.png)
    - Command: npm, start
    - Working directory: /usr/src/app (defined in Dockerfile) 
    - Environment variables: copy over whats in /api/.env
        - Select 'Value' in the middle dropdown for each environment variable
3. Click next on the Container and Task page.
4. Select Application Load Balancer on the Service page.
TODO: Add more pictures
- ![Alt text](config_3.png)
5. Rename the Cluster name and click next on the Cluster page.
- ![Alt text](config_4.png)
6. Click Create on the Review page.
7. Wait for components to be created then click 'View Service'
8. Add port 5000 to incoming traffic in security group if its not there already.
- ![Alt text](security_1.png)
    - View security group
- ![Alt text](security_2.png)
    - Add Inbound Rule if Port 5000 is not included

# Confirm Deployment
1. View the running Task 
- ![Alt text](task_1.png)
2. Open url to <PUBLIC_IP>:5000
- ![Alt text](task_2.png)
    - 54.202.141.14:5000
3. App should be running
- ![Alt text](task_3.png)

# Configure DNS
TODO: Add more pictures
1. Setup Route 53 to point to Public IP.
- https://itnext.io/run-your-containers-on-aws-fargate-c2d4f6a47fda