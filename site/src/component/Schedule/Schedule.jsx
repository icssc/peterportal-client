
import React, { useState, useEffect } from 'react';
import axios from 'axios';
import configData from "../../config.json";
import './Schedule.css';
import Table from 'react-bootstrap/Table';
import ProgressBar from 'react-bootstrap/ProgressBar';
import Button from 'react-bootstrap/ProgressBar';
import Col from 'react-bootstrap/Col';

var DUMMY_DATA = [
    {
        code: '35490',
        type: 'Lec',
        sec: 'A',
        instructor: ['BALDWIN, M.'],
        time: "MWF 3:00-3:50pm",
        place: "VRTL REMOTE",
        max: 90,
        enrolled: 64,
        waitlisted: 0,
        restriction: "A",
        status: "OPEN",
    },
    {
        code: '35491',
        type: 'Lab',
        sec: 'A1',
        instructor: ['REN, D.', 'BALDWIN, M.'],
        time: "TuTh 10:00-11:50pm",
        place: "VRTL REMOTE",
        max: 22,
        enrolled: 22,
        waitlisted: 3,
        restriction: "A",
        status: "WAITL",
    },
    {
        code: '35492',
        type: 'Lab',
        sec: 'A2',
        instructor: ['DIA, K.', 'BALDWIN, M.'],
        time: "TuTh 12:00-1:50pm",
        place: "VRTL REMOTE",
        max: 22,
        enrolled: 22,
        waitlisted: 0,
        restriction: "A",
        status: "FULL",
    },
    {
        code: '35493',
        type: 'Lab',
        sec: 'A3',
        instructor: ['DIA, K.', 'BALDWIN, M.'],
        time: "TuTh 2:00-3:50pm",
        place: "VRTL REMOTE",
        max: 22,
        enrolled: 14,
        waitlisted: 0,
        restriction: "A",
        status: "OPEN",
    }
]


export default function Schedule(props) {
    // For developing only
    const [scheduleData, setScheduleData] = useState(DUMMY_DATA);

    // For fetching data from API
    
    // const [scheduleData, setScheduleData] = useState(null);

    // useEffect(() => {
    //     fetchScheduleDataFromAPI();
    // }, [])

    // const fetchScheduleDataFromAPI = async () => {
    //     const apiResponse = await axios.get(`/courses/api/schedule/${configData.currentQuarter}/${props.department}/${props.number}`)
    //     setScheduleData(apiResponse.data)
    // }

    // if (!scheduleData) {
    //     return <p>Loading schedule..</p>;
    // } else {
    //     return (
    //         <div>
    //             {/* Mapping example, use table when creating this feature */}
    //             {scheduleData.map((e) => 
    //                 <div style={{marginBottom: "1rem"}}>
    //                     {e.code} {e.type + " " + e.sec} {e.instructor.map((i) => <div>{i}</div>)}
    //                 </div>
    //             )}
    //         </div>
    //     )
    // }
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
                    <ProgressBar variant= "success" now={course.enrolled *100 / course.max} />
                </div>
            )
        }
        else if (course.status == "WAITL"){
           return (
                    <div className="progress-bar">
                        <ProgressBar variant= "warning" now={course.enrolled *100 / course.max} />
                    </div>                    
            )
        }
        else{
            return (
                <div className="progress-bar">
                    <ProgressBar variant="danger" now={course.enrolled *100 / course.max} />
                </div>
            )
        }
    }


    const renderData = (course, index) => {
        //This function returns the data for a dynamic table after accessing the API
        return (
            <tr key ={index}>
                <td className = "data-col">{course.code}</td>
                <td className = "data-col">{course.type} {course.sec}</td>
                <td className = "data-col">{4}</td>
                <td className = "data-col">{course.instructor}</td>
                <td className = "data-col">{course.time}</td>
                <td className = "data-col">{course.place}</td>

                <td className = "enrollment-col">
                    <span className = "enrollment-info-text"> 
                        {course.enrolled} / {course.max} 
                    </span>
                    <span className = "enrollment-percentage">
                        {(course.enrolled *100 / course.max) >> 0}%
                    </span>

                    {renderProgressBar(course)}
                </td>
                    
                <td className = "data-col">{course.waitlisted}</td>
                <td className = "data-col">{course.restriction}</td>
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