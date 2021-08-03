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
3. Click next on the Container and Task page.
4. Use the defaults and click next on the Service page.
- ![Alt text](config_3.png)
5. Rename the Cluster name and click next on the Cluster page.
- ![Alt text](config_4.png)
6. Click Create on the Review page.
7. Add port 5000 to incoming traffic in security group.
8. Setup Route 53 to point to Public IP.