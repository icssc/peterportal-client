import './RequiredCourseList.scss';
import React, { FC, useCallback, useEffect, useState } from 'react';
import { collapseSingletonRequirements, COMPLETE_ALL_TEXT } from '../../../helpers/courseRequirements';
import { CaretDownFill, CaretRightFill } from 'react-bootstrap-icons';
import { CourseNameAndInfo } from '../Course';
import { CourseGQLData } from '../../../types/types';
import trpc from '../../../trpc';
import { programRequirementsSortable } from '../../../helpers/sortable';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { useIsMobile } from '../../../helpers/util';
import { setActiveCourse, setShowAddCourse } from '../../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../../store/hooks';
import { Spinner } from 'react-bootstrap';
import { ProgramRequirement, TypedProgramRequirement } from '@peterportal/types';

interface CourseTileProps {
  courseID: string;
  /** The timestamp at which the course data is requested to load */
  dragTimestamp?: number;
}

const CourseTile: FC<CourseTileProps> = ({ courseID, dragTimestamp = 0 }) => {
  const [courseData, setCourseData] = useState<string | CourseGQLData>(courseID);
  const [loading, setLoading] = useState(false);
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  const loadFullData = useCallback(async () => {
    if (typeof courseData !== 'string') return courseData;
    setLoading(true);
    const response = (await trpc.courses.get.query({ courseID })) as unknown as CourseGQLData;
    setCourseData(response);
    setLoading(false);
    return response;
  }, [courseData, courseID]);

  // Allows for dragging to trigger a data-load & setting active course
  useEffect(() => {
    if (!dragTimestamp) return;
    setLoading(true);
    loadFullData().then((res) => {
      dispatch(setActiveCourse(res));
      setLoading(false);
    });
  }, [dispatch, dragTimestamp, loadFullData]);

  const handlePopoverStateChange = (open: boolean) => open && loadFullData();

  const insertCourseOnClick = async () => {
    setLoading(true);
    const fullData = await loadFullData();
    dispatch(setActiveCourse(fullData as CourseGQLData));
    dispatch(setShowAddCourse(true));
    setLoading(false);
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = isMobile ? tapProps : {};
  const className = `program-course-tile ${isMobile ? 'mobile' : 'none'} ${loading ? 'loading' : ''}`;

  return (
    <div className={className} {...tappableCourseProps}>
      <CourseNameAndInfo data={courseData} openPopoverLeft popupListener={handlePopoverStateChange} alwaysCollapse />
      {isMobile && loading && <Spinner animation="border" />}
    </div>
  );
};

const CourseList: FC<{ courses: string[] }> = ({ courses }) => {
  const isMobile = useIsMobile();
  const [timestamps, setTimestamps] = useState<number[]>(new Array(courses.length).fill(0));

  const setDraggedItem = async (event: SortableEvent) => {
    timestamps[event.oldIndex!] = Date.now();
    setTimestamps(timestamps.slice());
  };

  const courseIDs = courses.map((c) => ({ id: c }));
  return (
    <ReactSortable
      {...programRequirementsSortable}
      list={courseIDs}
      onStart={setDraggedItem}
      disabled={isMobile}
      className={'group-courses' + (isMobile ? ' disabled' : '')}
    >
      {courses.map((c, i) => (
        <CourseTile courseID={c} key={c} dragTimestamp={timestamps[i]} />
      ))}
    </ReactSortable>
  );
};

interface GroupHeaderProps {
  title: string;
  open: boolean;
  setOpen: React.Dispatch<boolean>;
}
const GroupHeader: FC<GroupHeaderProps> = ({ title, open, setOpen }) => {
  const className = `group-header ${open ? 'open' : ''}`;
  return (
    <button className={className} onClick={() => setOpen(!open)}>
      {open ? <CaretDownFill /> : <CaretRightFill />}
      <b>{title}</b>
    </button>
  );
};

const CourseRequirement: FC<{ data: TypedProgramRequirement<'Course'> }> = ({ data }) => {
  const [open, setOpen] = useState(false);

  const requiredAmount = data.courseCount === data.courses.length ? 'all' : data.courseCount;
  const showLabel = data.courses.length > 1 && data.label !== COMPLETE_ALL_TEXT;
  return (
    <div className="group-requirement">
      <GroupHeader title={data.label} open={open} setOpen={setOpen} />
      {open && showLabel && (
        <p>
          <b>Complete {requiredAmount} of the following:</b>
        </p>
      )}
      {open && <CourseList courses={data.courses} />}
    </div>
  );
};

const GroupedCourseRequirement: FC<{ data: TypedProgramRequirement<'Course'> }> = ({ data }) => {
  return (
    <div className="course-requirement">
      <CourseList courses={data.courses} />
    </div>
  );
};

const GroupRequirement: FC<{ data: TypedProgramRequirement<'Group'> }> = ({ data }) => {
  const [open, setOpen] = useState(false);
  return (
    <div className="group-requirement">
      <GroupHeader title={data.label} open={open} setOpen={setOpen} />
      {open && (
        <p>
          Complete <b>{data.requirementCount}</b> of the following series:
        </p>
      )}
      {open &&
        data.requirements.map((r, i) => (
          <React.Fragment key={i}>
            <p>
              <b>{r.label}</b>
            </p>
            <ProgramRequirementDisplay requirement={r} nested />
          </React.Fragment>
        ))}
    </div>
  );
};

interface ProgramRequirementDisplayProps {
  requirement: ProgramRequirement;
  nested?: boolean;
}
const ProgramRequirementDisplay: FC<ProgramRequirementDisplayProps> = ({ requirement, nested }) => {
  switch (requirement.requirementType) {
    case 'Course': {
      const DisplayComponent = nested ? GroupedCourseRequirement : CourseRequirement;
      return <DisplayComponent data={requirement} />;
    }
    case 'Group':
      return <GroupRequirement data={requirement} />;
    case 'Unit':
      return <></>;
  }
};

interface RequireCourseListProps {
  requirements: ProgramRequirement[];
}

const ProgramRequirementsList: FC<RequireCourseListProps> = ({ requirements }) => {
  const collapsedRequirements = collapseSingletonRequirements(requirements);

  return (
    <div className="program-requirements">
      {/* key is ok because we don't reorder these */}
      {collapsedRequirements.map((r, i) => (
        <ProgramRequirementDisplay requirement={r} key={i} />
      ))}
    </div>
  );
};

export default ProgramRequirementsList;
