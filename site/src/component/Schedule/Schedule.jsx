
import React, { useState, useEffect } from 'react';
import axios from 'axios';
import configData from "../../config.json";
import './Schedule.css';
import Table from 'react-bootstrap/Table';
import ProgressBar from 'react-bootstrap/ProgressBar';
import Button from 'react-bootstrap/ProgressBar';
import Col from 'react-bootstrap/Col';




export default function Schedule(props) {
    // For developing only
    //const [scheduleData, setScheduleData] = useState(DUMMY_DATA);


    // For fetching data from API
    
    const [scheduleData, setScheduleData] = useState(null);

    useEffect(() => {
         fetchScheduleDataFromAPI();
     }, [])

     const currentQuarter = '2020 Winter';
     const department = 'I&C SCI';
     const courseNum = '139W';

     const fetchScheduleDataFromAPI = async () => {
        const apiResponse = await axios.get(`/schedule/api/${currentQuarter}/${department}/${courseNum}`)
        setScheduleData(apiResponse.data);
        console.log(apiResponse.data);
     }

    

    const renderButton = (course) =>{
        //Renders the button which displays the status of the course. e.g: "OPEN", "FULL", "WAITLISTED"
        if (course.status == "OPEN"){
           return(
            <Button variant="light" size='lg' className="btn-status-button-open"> OPEN </Button>
           )
        }
        else if (course.status == "WAITL"){
            return (
                <Button variant="light" size='lg' className="btn-status-button-waitl"> WAITLIST </Button>
            )
        }
        else{
            return (
                <Button variant="light" size='lg' className="btn-status-button-full"> FULL </Button>
            )
        }
    }



    const renderProgressBar = (course) => {
        //This function returns the progress Bar for the enrollment into the class.
        if (course.status == "OPEN"){
            return (
                <div className="progress-bar">
                    <ProgressBar variant= "success" now={course.numCurrentlyEnrolled.totalEnrolled *100 / course.maxCapacity} />
                </div>
            )
        }
        else if (course.status == "WAITL"){
           return (
                    <div className="progress-bar">
                        <ProgressBar variant= "warning" now={course.numCurrentlyEnrolled.totalEnrolled *100 / course.maxCapacity} />
                    </div>                    
            )
        }
        else{
            return (
                <div className="progress-bar">
                    <ProgressBar variant="danger" now={course.numCurrentlyEnrolled.totalEnrolled *100 / course.maxCapacity} />
                </div>
            )
        }
    }


    const renderData = (course, index) => {
        //This function returns the data for a dynamic table after accessing the API
        return (
            <tr key ={index}>
                <td className = "data-col">{course.sectionCode}</td>
                <td className = "data-col">{course.sectionType} {course.sectionNum}</td>
                <td className = "data-col">{4}</td>
                <td className = "data-col">{course.instructors[0]}</td>
                <td className = "data-col">{course.meetings[0].time}</td>
                <td className = "data-col">{course.meetings[0].bldg}</td>

                <td className = "enrollment-col">
                    <span className = "enrollment-info-text"> 
                        {course.numCurrentlyEnrolled.totalEnrolled} / {course.maxCapacity} 
                    </span>
                    <span className = "enrollment-percentage">
                        {(course.numCurrentlyEnrolled.totalEnrolled *100 / course.maxCapacity) >> 0}%
                    </span>

                    {renderProgressBar(course)}
                </td>
                    
                <td className = "data-col">{course.numOnWaitlist}</td>
                <td className = "data-col">{course.restrictions}</td>
                <td className = "data-col">
                    {renderButton(course)}
                </td>
            </tr>
        )
    }

    if (!scheduleData){
        return <p> Loading Schedule..</p>;
    }else{
        return (
            <div>
                <Col className="col-tableHolder">
                <Table responsive borderless className="schedule-table">
                 <thead>
                     <tr>
                     <th> Code </th>
                     <th> Section </th>
                     <th> Units </th>
                     <th> Instructor </th>
                     <th> Time </th>
                     <th> Place </th>
                     <th className = "enrollment-col"> Enrollment </th>
                     <th> WL </th>
                     <th> Rstr </th>
                     <th> Status </th>
                     </tr>
                 </thead>  
                 <tbody>

                 {scheduleData.map(renderData)}

                </tbody> 
                </Table>
                </Col>
            </div>
        )
    }
}