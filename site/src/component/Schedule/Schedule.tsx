
import React, { FC, useState, useEffect } from 'react';
import axios, { AxiosResponse } from 'axios';
import './Schedule.css';
import Table from 'react-bootstrap/Table';
import ProgressBar from 'react-bootstrap/ProgressBar';
import Button from 'react-bootstrap/Button';
import Col from 'react-bootstrap/Col';

import { WebsocResponse, Section } from 'websoc-api';
import { CourseData } from 'src/types/types';

interface ScheduleProps {
    courseID?: string;
}

const Schedule: FC<ScheduleProps> = (props) => {
    // For fetching data from API
    const [scheduleData, setScheduleData] = useState<Section[]>(null!);

    const currentQuarter = '2020 Winter';
    useEffect(() => {
        fetchScheduleDataFromAPI(currentQuarter);
    }, [])


    const fetchScheduleDataFromAPI = async (currentQuarter: string) => {
        let department;
        let number;
        if (props.courseID) {
            const str = props.courseID.split(' ');
            department = str.slice(0, str.length - 1).join(' ');
            number = str[str.length - 1];
        }
        const apiResponse: AxiosResponse<WebsocResponse> = await axios.get(`/schedule/api/${currentQuarter}/${department}/${number}`);
        try {
            let sections = apiResponse.data.schools[0].departments[0].courses[0].sections;
            setScheduleData(sections);
        }
        catch (error) {
            // No school/department/course
            if (error instanceof TypeError) {
                setScheduleData([]);
            }
        }
    }

    const renderButton = (course: Section) => {
        //Renders the button which displays the status of the course. e.g: 'OPEN', 'FULL', 'WAITLISTED'
        if (course.status == 'OPEN') {
            return (
                // @ts-ignore
                <Button variant='light' size='lg' className='btn-status-button-open btn-status' disabled={true}> OPEN </Button>
            )
        }
        else if (course.status == 'WAITL') {
            return (
                // @ts-ignore
                <Button variant='light' size='lg' className='btn-status-button-waitl btn-status' disabled={true}> WAITLIST </Button>
            )
        }
        else {
            return (
                // @ts-ignore
                <Button variant='light' size='lg' className='btn-status-button-full btn-status' disabled={true}> FULL </Button>
            )
        }
    }



    const renderProgressBar = (course: Section) => {
        //This function returns the progress Bar for the enrollment into the class.
        let percentage = Number(course.numCurrentlyEnrolled.totalEnrolled) * 100 / Number(course.maxCapacity);
        if (course.status == 'OPEN') {
            return (
                <div className='progress-bar'>
                    <ProgressBar variant='success' now={percentage} />
                </div>
            )
        }
        else if (course.status == 'WAITL') {
            return (
                <div className='progress-bar'>
                    <ProgressBar variant='warning' now={percentage} />
                </div>
            )
        }
        else {
            return (
                <div className='progress-bar'>
                    <ProgressBar variant='danger' now={percentage} />
                </div>
            )
        }
    }

    const renderData = (course: Section, index: number) => {
        //This function returns the data for a dynamic table after accessing the API
        return (
            <tr key={index}>
                <td className='data-col'>{course.sectionCode}</td>
                <td className='data-col'>{course.sectionType} {course.sectionNum}</td>
                <td className='data-col'>{course.units}</td>
                <td className='data-col'>{course.instructors[0]}</td>
                <td className='data-col'>{course.meetings[0].time}</td>
                <td className='data-col'>{course.meetings[0].bldg}</td>

                <td className='enrollment-col'>
                    <span className='enrollment-info-text'>
                        {Number(course.numCurrentlyEnrolled.totalEnrolled)} / {Number(course.maxCapacity)}
                    </span>
                    <span className='enrollment-percentage'>
                        {(Number(course.numCurrentlyEnrolled.totalEnrolled) * 100 / Number(course.maxCapacity)) >> 0}%
                    </span>

                    {renderProgressBar(course)}
                </td>

                <td className='data-col'>{course.numOnWaitlist}</td>
                <td className='data-col'>{course.restrictions}</td>
                <td className='data-col'>
                    {renderButton(course)}
                </td>
            </tr>
        )
    }

    if (!scheduleData) {
        return <p> Loading Schedule..</p>;
    } else {
        return (
            <div>
                <Col className='col-tableHolder'>
                    <Table responsive borderless className='schedule-table'>
                        <thead>
                            <tr>
                                <th> Code </th>
                                <th> Section </th>
                                <th> Units </th>
                                <th> Instructor </th>
                                <th> Time </th>
                                <th> Place </th>
                                <th className='enrollment-col'> Enrollment </th>
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

export default Schedule;