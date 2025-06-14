import { FC, useState, useEffect, useCallback, useContext } from 'react';
import './Schedule.scss';
import { Dropdown, DropdownButton } from 'react-bootstrap';
import ProgressBar from 'react-bootstrap/ProgressBar';

import { WebsocAPIResponse, WebsocSection as Section } from '@peterportal/types';
import { CourseGQLData, DataType, ProfessorGQLData } from '../../types/types';
import { hourMinuteTo12HourString } from '../../helpers/util';
import trpc from '../../trpc';
import ThemeContext from '../../style/theme-context';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';

interface TableRowProps {
  dataType: DataType;
  courseID: string;
  section: Section;
  index: number;
}

const TableRow: FC<TableRowProps> = ({ dataType, courseID, section, index }) => {
  const getMeetingsString = (section: Section) => {
    const meetingStrings = section.meetings.map((meeting) => {
      if (meeting.timeIsTBA) return 'TBA';
      const { days, startTime, endTime } = meeting;
      const start = hourMinuteTo12HourString(startTime);
      const end = hourMinuteTo12HourString(endTime);
      return `${days} \n${start}-\n${end}`;
    });
    if (meetingStrings.length === 1) return meetingStrings[0];
    return meetingStrings.map((str) => str.replace(/\n/g, '')).join('\n');
  };

  const currentlyEnrolled = parseInt(section.numCurrentlyEnrolled.totalEnrolled);
  const enrollmentPercent = (currentlyEnrolled * 100) / parseInt(section.maxCapacity);

  section.status = section.status || 'FULL';

  return (
    <tr key={index}>
      {dataType === 'professor' && <td className="data-col">{courseID}</td>}
      <td className="data-col">{section.sectionCode}</td>
      <td className="data-col">
        {section.sectionType} {section.sectionNum}
      </td>
      <td className="data-col">{section.units}</td>
      <td className="data-col">{section.instructors.join('\n')}</td>
      <td className="data-col">{getMeetingsString(section)}</td>
      <td className="data-col">
        {section.meetings.map((meeting) => (meeting.timeIsTBA ? ['TBA'] : meeting.bldg)).join('\n')}
      </td>

      <td className="enrollment-col">
        <div className="capacity-text">
          <span className="enrollment-info-text">
            {currentlyEnrolled} / {section.maxCapacity}
          </span>
          <span className="enrollment-percentage">{Math.round(enrollmentPercent)}%</span>
        </div>
        <div className="progress-bar">
          <ProgressBar now={enrollmentPercent} data-status={section.status} />
        </div>
      </td>

      <td className="data-col">{section.numOnWaitlist}</td>
      <td className="data-col">{section.restrictions}</td>
      <td className="data-col">
        <div className="status-badge" data-status={section.status}>
          {section.status}
        </div>
      </td>
    </tr>
  );
};

interface ScheduleData {
  [key: string]: Section[];
}

interface ScheduleProps {
  dataType: DataType;
  data: CourseGQLData | ProfessorGQLData;
  terms: string[];
}

const Schedule: FC<ScheduleProps> = ({ dataType, data, terms }) => {
  const [scheduleData, setScheduleData] = useState<ScheduleData>({});
  const [currentQuarter, setCurrentQuarter] = useState('');
  const [selectedQuarter, setSelectedQuarter] = useState('');
  const { darkMode } = useContext(ThemeContext);

  useEffect(() => {
    // get the current quarter used in websoc
    trpc.schedule.currentQuarter.query().then((data) => {
      // use it as the default in the dropdown
      setCurrentQuarter(data);
      setSelectedQuarter(data);
    });
  }, []);

  const fetchScheduleDataFromAPI = useCallback(async () => {
    const apiResponse: WebsocAPIResponse =
      dataType === 'course'
        ? await trpc.schedule.getTermDeptNum.query({
            term: selectedQuarter,
            department: (data as CourseGQLData).department,
            number: (data as CourseGQLData).courseNumber,
          })
        : {
            schools: (
              await Promise.all(
                (data as ProfessorGQLData).shortenedNames.map((professor) =>
                  trpc.schedule.getTermProf.query({ term: selectedQuarter, professor }),
                ),
              )
            ).flatMap((r) => r.schools),
          };
    const scheduleData: ScheduleData = {};
    apiResponse.schools?.forEach((school) => {
      school.departments?.forEach((department) => {
        department.courses?.forEach((course) => {
          scheduleData[department.deptCode + course.courseNumber] = course.sections;
        });
      });
    });
    setScheduleData(scheduleData);
  }, [dataType, data, selectedQuarter]);

  useEffect(() => {
    if (!selectedQuarter) return;
    fetchScheduleDataFromAPI();
  }, [selectedQuarter, fetchScheduleDataFromAPI]);

  if (!scheduleData) {
    return <LoadingSpinner />;
  }

  return (
    <div>
      {terms.length === 0 ? (
        <div className="schedule-quarter">Showing results for {selectedQuarter}</div>
      ) : (
        <DropdownButton
          className="ppc-dropdown-btn"
          title={selectedQuarter ?? currentQuarter}
          variant={darkMode ? 'dark' : 'light'}
          onSelect={(value) => setSelectedQuarter(value!)}
        >
          {terms.map((term) => (
            <Dropdown.Item key={term} eventKey={term}>
              {term}
            </Dropdown.Item>
          ))}
        </DropdownButton>
      )}
      <div className="table-wrapper">
        <table className="ppc-table schedule-table">
          <thead>
            <tr>
              {dataType === 'professor' && <th>Course</th>}
              <th>Code</th>
              <th>Section</th>
              <th>Units</th>
              <th>Instructor</th>
              <th>Time</th>
              <th>Place</th>
              <th className="enrollment-col">Enrollment</th>
              <th>WL</th>
              <th>Rstr</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(scheduleData).flatMap(([courseID, sections]) => {
              return sections.map((section, i) => (
                <TableRow key={i} dataType={dataType} courseID={courseID} section={section} index={i} />
              ));
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default Schedule;
