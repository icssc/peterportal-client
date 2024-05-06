import { FC, useState, useEffect } from 'react';
import axios, { AxiosResponse } from 'axios';
import './Schedule.css';
import Table from 'react-bootstrap/Table';
import ProgressBar from 'react-bootstrap/ProgressBar';
import Button from 'react-bootstrap/Button';

import { WebsocAPIResponse as WebsocResponse, WebsocSection as Section } from 'peterportal-api-next-types';
import { hourMinuteTo12HourString } from '../../helpers/util';
import { Dropdown } from 'semantic-ui-react';

interface ScheduleProps {
  courseID?: string;
  professorID?: string;
  termsOffered?: string[];
}

interface ScheduleData {
  [key: string]: Section[];
}

const Schedule: FC<ScheduleProps> = (props) => {
  // For fetching data from API
  const [scheduleData, setScheduleData] = useState<ScheduleData>(null!);
  const [currentQuarter, setCurrentQuarter] = useState<string>('');
  const [selectedQuarter, setSelectedQuarter] = useState<string>('');

  useEffect(() => {
    // get the current quarter used in websoc
    axios.get<string>('/api/schedule/api/currentQuarter').then((res) => {
      // use it as the default in the dropdown
      setCurrentQuarter(res.data);
      setSelectedQuarter(res.data);
    });
  }, []);

  useEffect(() => {
    if (selectedQuarter !== '') {
      fetchScheduleDataFromAPI(selectedQuarter);
    }
  }, [selectedQuarter, props.courseID, props.professorID]);

  const fetchScheduleDataFromAPI = async (selectedQuarter: string) => {
    let url = '';
    if (props.courseID) {
      const courseIDSplit = props.courseID.split(' ');
      const department = courseIDSplit.slice(0, courseIDSplit.length - 1).join(' ');
      const number = courseIDSplit[courseIDSplit.length - 1];

      url = `/api/schedule/api/${selectedQuarter}/${department}/${number}`;
    } else if (props.professorID) {
      url = `/api/schedule/api/${selectedQuarter}/${props.professorID}`;
    }

    const apiResponse: AxiosResponse<WebsocResponse> = await axios.get(url);
    try {
      const data: ScheduleData = {};
      apiResponse.data.schools.forEach((school) => {
        school.departments.forEach((department) => {
          department.courses.forEach((course) => {
            data[department.deptCode + course.courseNumber] = course.sections;
          });
        });
      });
      setScheduleData(data);
    } catch (error) {
      // No school/department/course
      if (error instanceof TypeError) {
        setScheduleData({});
      }
    }
  };

  const renderButton = (section: Section) => {
    //Renders the button which displays the status of the course. e.g: 'OPEN', 'FULL', 'WAITLISTED'
    if (section.status == 'OPEN') {
      return (
        <Button variant="light" size="lg" className="btn-status-button-open btn-status" disabled={true}>
          {' '}
          OPEN{' '}
        </Button>
      );
    } else if (section.status == 'Waitl') {
      return (
        <Button variant="light" size="lg" className="btn-status-button-waitl btn-status" disabled={true}>
          {' '}
          WAITLIST{' '}
        </Button>
      );
    } else {
      return (
        <Button variant="light" size="lg" className="btn-status-button-full btn-status" disabled={true}>
          {' '}
          FULL{' '}
        </Button>
      );
    }
  };

  const renderProgressBar = (section: Section) => {
    //This function returns the progress Bar for the enrollment into the class.
    const percentage = (Number(section.numCurrentlyEnrolled.totalEnrolled) * 100) / Number(section.maxCapacity);
    if (section.status == 'OPEN') {
      return (
        <div className="progress-bar">
          <ProgressBar variant="success" now={percentage} />
        </div>
      );
    } else if (section.status == 'Waitl') {
      return (
        <div className="progress-bar">
          <ProgressBar variant="warning" now={percentage} />
        </div>
      );
    } else {
      return (
        <div className="progress-bar">
          <ProgressBar variant="danger" now={percentage} />
        </div>
      );
    }
  };

  const renderData = (courseID: string, section: Section, index: number) => {
    //This function returns the data for a dynamic table after accessing the API
    return (
      <tr key={index}>
        {props.professorID && <td className="data-col">{courseID}</td>}
        <td className="data-col">{section.sectionCode}</td>
        <td className="data-col">
          {section.sectionType} {section.sectionNum}
        </td>
        <td className="data-col">{section.units}</td>
        <td className="data-col">{section.instructors.join('\n')}</td>
        <td className="data-col">
          {section.meetings
            .map((meeting) =>
              meeting.timeIsTBA
                ? 'TBA'
                : `${meeting.days} ${hourMinuteTo12HourString(meeting.startTime!)} - ${hourMinuteTo12HourString(
                    meeting.endTime!,
                  )}`,
            )
            .join('\n')}
        </td>
        <td className="data-col">{section.meetings.map((meeting) => meeting.bldg).join('\n')}</td>

        <td className="enrollment-col">
          <span className="enrollment-info-text">
            {Number(section.numCurrentlyEnrolled.totalEnrolled)} / {Number(section.maxCapacity)}
          </span>
          <span className="enrollment-percentage">
            {((Number(section.numCurrentlyEnrolled.totalEnrolled) * 100) / Number(section.maxCapacity)) >> 0}%
          </span>

          {renderProgressBar(section)}
        </td>

        <td className="data-col">{section.numOnWaitlist}</td>
        <td className="data-col">{section.restrictions}</td>
        <td className="data-col">{renderButton(section)}</td>
      </tr>
    );
  };

  if (!scheduleData) {
    return <p> Loading Schedule..</p>;
  } else {
    const sectionElements: JSX.Element[] = [];
    Object.keys(scheduleData).forEach((courseID) => {
      scheduleData[courseID].forEach((section, i) => {
        sectionElements.push(renderData(courseID, section, i));
      });
    });

    return (
      <div>
        {props.termsOffered ? (
          <Dropdown
            placeholder={currentQuarter}
            scrolling
            selection
            options={
              // in dropdown options, prepend current quarter to the list of past terms
              [
                { text: currentQuarter, value: currentQuarter },
                ...props.termsOffered.map((term) => {
                  return {
                    text: term,
                    value: term,
                  };
                }),
              ]
            }
            value={selectedQuarter}
            onChange={(_, s) => setSelectedQuarter(s.value as string)}
          />
        ) : (
          <div className="schedule-quarter">Showing results for {selectedQuarter}</div>
        )}
        <Table responsive borderless className="schedule-table">
          <thead>
            <tr>
              {props.professorID && <th> Course </th>}
              <th> Code </th>
              <th> Section </th>
              <th> Units </th>
              <th> Instructor </th>
              <th> Time </th>
              <th> Place </th>
              <th className="enrollment-col"> Enrollment </th>
              <th> WL </th>
              <th> Rstr </th>
              <th> Status </th>
            </tr>
          </thead>
          <tbody>{sectionElements}</tbody>
        </Table>
      </div>
    );
  }
};

export default Schedule;
