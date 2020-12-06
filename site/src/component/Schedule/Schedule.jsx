
import React, { useState, useEffect } from 'react';
import axios from 'axios';
import configData from "../../config.json";
import './Schedule.scss';

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

    if (!scheduleData) {
        return <p>Loading schedule..</p>;
    } else {
        return (
            <div>
                {/* Mapping example, use table when creating this feature */}
                {scheduleData.map((e) => 
                    <div style={{marginBottom: "1rem"}}>
                        {e.code} {e.type + " " + e.sec} {e.instructor.map((i) => <div>{i}</div>)}
                    </div>
                )}
            </div>
        )
    }
}