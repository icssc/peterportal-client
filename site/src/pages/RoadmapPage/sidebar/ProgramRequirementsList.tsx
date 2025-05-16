import './ProgramRequirementsList.scss';
import React, { FC, useCallback, useEffect, useState } from 'react';
import {
  COMPLETE_ALL_TEXT,
  CompletedCourseSet,
  formatRequirements,
  LOADING_COURSE_PLACEHOLDER,
  saveMarkerCompletion,
  useCompletionCheck,
} from '../../../helpers/courseRequirements';
import { CaretDownFill, CaretRightFill } from 'react-bootstrap-icons';
import { CourseNameAndInfo } from '../Course';
import { CourseGQLData } from '../../../types/types';
import trpc from '../../../trpc';
import { programRequirementsSortable } from '../../../helpers/sortable';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { useIsMobile } from '../../../helpers/util';
import {
  setActiveCourse,
  setActiveCourseLoading,
  setActiveMissingPrerequisites,
  setShowAddCourse,
} from '../../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { Spinner } from 'react-bootstrap';
import { ProgramRequirement } from '@peterportal/types';
import { setGroupExpanded, setMarkerComplete } from '../../../store/slices/courseRequirementsSlice';
import { getMissingPrerequisites } from '../../../helpers/planner';
import { useClearedCourses } from '../../../hooks/planner';
import { useTransferredCredits } from '../../../hooks/transferCredits';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';

interface CompletedCourseSetWithType {
  [k: string]: {
    units: number;
    transferType: 'AP' | 'Course';
  };
}

interface CourseTileProps {
  courseID: string;
  /** The timestamp at which the course data is requested to load */
  dragTimestamp?: number;
  taken?: boolean;
  transferredByAP?: boolean;
  transferredByCourse?: boolean;
}
const CourseTile: FC<CourseTileProps> = ({
  courseID,
  dragTimestamp = 0,
  taken,
  transferredByAP,
  transferredByCourse,
}) => {
  const [courseData, setCourseData] = useState<string | CourseGQLData>(courseID);
  const [loading, setLoading] = useState(false);
  const isMobile = useIsMobile();
  const clearedCourses = useClearedCourses();
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
    dispatch(setActiveCourse(LOADING_COURSE_PLACEHOLDER));
    dispatch(setActiveCourseLoading(true));
    loadFullData().then((res) => {
      dispatch(setActiveCourse(res));
      dispatch(setActiveCourseLoading(false));
      setLoading(false);
    });
  }, [dispatch, dragTimestamp, loadFullData]);

  const handlePopoverStateChange = (open: boolean) => open && loadFullData();

  const insertCourseOnClick = async () => {
    setLoading(true);
    const fullData = await loadFullData();
    const missingPrerequisites = getMissingPrerequisites(clearedCourses, fullData);
    dispatch(setActiveCourse(fullData as CourseGQLData));
    dispatch(setActiveMissingPrerequisites(missingPrerequisites));
    dispatch(setShowAddCourse(true));
    setLoading(false);
  };

  const tapProps = { onClick: insertCourseOnClick, role: 'button', tabIndex: 0 };
  const tappableCourseProps = isMobile ? tapProps : {};
  const className = `program-course-tile${isMobile ? ' mobile' : ''}${loading ? ' loading' : ''}${taken ? ' completed' : ''}`;
  let fontSize: string | undefined;

  if (courseID.length > 10) {
    const charsExtra = courseID.length - 10;
    const computedSize = 13 - charsExtra;
    fontSize = computedSize + 'px';
  }

  return (
    <div className={className} {...tappableCourseProps} style={{ fontSize }}>
      {transferredByAP && <div className="source-overlay">AP</div>}
      {transferredByCourse && <div className="source-overlay">Course</div>}
      <CourseNameAndInfo data={courseData} openPopoverLeft popupListener={handlePopoverStateChange} alwaysCollapse />
      {isMobile && loading && <Spinner animation="border" />}
    </div>
  );
};

interface CourseListProps {
  courses: string[];
  takenCourseIDs: CompletedCourseSet;
  transferredCourseIDs: CompletedCourseSetWithType;
}
const CourseList: FC<CourseListProps> = ({ courses, takenCourseIDs, transferredCourseIDs }) => {
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
        <CourseTile
          courseID={c}
          key={c}
          dragTimestamp={timestamps[i]}
          taken={c in takenCourseIDs}
          transferredByAP={c in transferredCourseIDs && transferredCourseIDs[c].transferType === 'AP'}
          transferredByCourse={c in transferredCourseIDs && transferredCourseIDs[c].transferType === 'Course'}
        />
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

interface CourseRequirementProps {
  data: ProgramRequirement<'Course' | 'Unit'>;
  takenCourseIDs: CompletedCourseSet;
  transferredCourseIDs: CompletedCourseSetWithType;
  storeKey: string;
}
const CourseRequirement: FC<CourseRequirementProps> = ({ data, takenCourseIDs, transferredCourseIDs, storeKey }) => {
  const dispatch = useAppDispatch();
  const complete = useCompletionCheck(takenCourseIDs, data).done;

  const open = useAppSelector((state) => state.courseRequirements.expandedGroups[storeKey] ?? false);

  const setOpen = (isOpen: boolean) => {
    dispatch(setGroupExpanded({ storeKey: storeKey, expanded: isOpen }));
  };

  let label: string | number;
  if ('courseCount' in data) {
    label = data.courseCount === data.courses.length ? 'all' : data.courseCount;
  } else {
    label = data.unitCount + ' units';
  }
  const showLabel = data.courses.length > 1 && data.label !== COMPLETE_ALL_TEXT;
  const className = `group-requirement${complete ? ' completed' : ''}`;

  return (
    <div className={className}>
      <GroupHeader title={data.label} open={open} setOpen={setOpen} />
      {open && showLabel && (
        <p className="requirement-label">
          <b>Complete {label} of the following:</b>
        </p>
      )}
      {open && (
        <CourseList
          courses={data.courses}
          takenCourseIDs={takenCourseIDs}
          transferredCourseIDs={transferredCourseIDs}
        />
      )}
    </div>
  );
};

interface GroupedCourseRequirementProps {
  data: ProgramRequirement<'Course' | 'Unit'>;
  takenCourseIDs: CompletedCourseSet;
  transferredCourseIDs: CompletedCourseSetWithType;
}
const GroupedCourseRequirement: FC<GroupedCourseRequirementProps> = ({
  data,
  takenCourseIDs,
  transferredCourseIDs,
}) => {
  const complete = useCompletionCheck(takenCourseIDs, data).done;
  const className = `course-requirement${complete ? ' completed' : ''}`;

  return (
    <>
      <div className={className}>
        <p className="requirement-label">
          <b>{data.label}</b>
        </p>
        <CourseList
          courses={data.courses}
          takenCourseIDs={takenCourseIDs}
          transferredCourseIDs={transferredCourseIDs}
        />
      </div>
    </>
  );
};

interface GroupRequirementProps {
  data: ProgramRequirement<'Group'>;
  takenCourseIDs: CompletedCourseSet;
  transferredCourseIDs: CompletedCourseSetWithType;
  storeKey: string;
}
const GroupRequirement: FC<GroupRequirementProps> = ({ data, takenCourseIDs, transferredCourseIDs, storeKey }) => {
  const complete = useCompletionCheck(takenCourseIDs, data).done;
  const open = useAppSelector((state) => state.courseRequirements.expandedGroups[storeKey] ?? false);
  const dispatch = useAppDispatch();

  const setOpen = (isOpen: boolean) => {
    dispatch(setGroupExpanded({ storeKey: storeKey, expanded: isOpen }));
  };

  const className = `group-requirement${complete ? ' completed' : ''}`;

  return (
    <div className={className}>
      <GroupHeader title={data.label} open={open} setOpen={setOpen} />
      {open && (
        <p className="requirement-label">
          Complete <b>{data.requirementCount}</b> of the following series:
        </p>
      )}
      {open &&
        data.requirements.map((r, i) => (
          <ProgramRequirementDisplay
            key={i}
            storeKey={`${storeKey}-${i}`}
            requirement={r}
            nested
            takenCourseIDs={takenCourseIDs}
            transferredCourseIDs={transferredCourseIDs}
          />
        ))}
    </div>
  );
};

interface MarkerRequirementProps {
  data: ProgramRequirement<'Marker'>;
  storeKey: string;
}
const MarkerRequirement: FC<MarkerRequirementProps> = ({ data, storeKey }) => {
  const complete = useAppSelector((state) => state.courseRequirements.completedMarkers[data.label]) ?? false;
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();

  const setComplete = (complete: boolean) => {
    saveMarkerCompletion(data.label, complete, isLoggedIn);
    return dispatch(setMarkerComplete({ markerName: data.label, complete }));
  };

  const className = `marker-requirement${complete ? ' completed' : ''}`;

  return (
    <div className={className}>
      <label>
        <b>{data.label}</b>
        <input
          type="checkbox"
          name={'marker-' + storeKey}
          className="form-check-input"
          checked={complete}
          onChange={(e) => setComplete(e.target.checked)}
        />
      </label>
    </div>
  );
};

interface ProgramRequirementDisplayProps {
  requirement: ProgramRequirement;
  nested?: boolean;
  takenCourseIDs: CompletedCourseSet;
  transferredCourseIDs: CompletedCourseSetWithType;
  storeKey: string;
}
const ProgramRequirementDisplay: FC<ProgramRequirementDisplayProps> = ({
  requirement,
  nested,
  takenCourseIDs,
  transferredCourseIDs,
  storeKey,
}) => {
  switch (requirement.requirementType) {
    case 'Unit':
    case 'Course': {
      return nested ? (
        <GroupedCourseRequirement
          data={requirement}
          takenCourseIDs={takenCourseIDs}
          transferredCourseIDs={transferredCourseIDs}
        />
      ) : (
        <CourseRequirement
          data={requirement}
          storeKey={storeKey}
          takenCourseIDs={takenCourseIDs}
          transferredCourseIDs={transferredCourseIDs}
        />
      );
    }
    case 'Group':
      return (
        <GroupRequirement
          data={requirement}
          storeKey={storeKey}
          takenCourseIDs={takenCourseIDs}
          transferredCourseIDs={transferredCourseIDs}
        />
      );
    case 'Marker':
      return <MarkerRequirement data={requirement} storeKey={storeKey} />;
  }
};

interface RequireCourseListProps {
  requirements: ProgramRequirement[];
  storeKeyPrefix: string;
}
const ProgramRequirementsList: FC<RequireCourseListProps> = ({ requirements, storeKeyPrefix }) => {
  const formattedRequirements = formatRequirements(requirements);
  const transferredCourses = useTransferredCredits().courses;
  const roadmapPlans = useAppSelector((state) => state.roadmap.plans);
  const roadmapPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const yearPlans = roadmapPlans[roadmapPlanIndex].content.yearPlans;

  const roadmapCourseMap = yearPlans
    .flatMap((year) => year.quarters)
    .flatMap((quarter) => quarter.courses)
    .map((course) => [course.id, course.minUnits]);
  const transferCourseMap = transferredCourses.map((t) => [t.courseName.replace(/\s/g, ''), t.units ?? 0]);
  const transferCourseMapWithType = transferredCourses.map((t) => [
    t.courseName.replace(/\s/g, ''),
    { units: t.units ?? 0, transferType: t.transferType },
  ]);

  const takenCourseSet: CompletedCourseSet = Object.assign(
    {},
    Object.fromEntries(roadmapCourseMap),
    Object.fromEntries(transferCourseMap),
  );
  const transferredCourseSet: CompletedCourseSetWithType = Object.fromEntries(transferCourseMapWithType);

  return (
    <div className="program-requirements">
      {/* key is ok because we don't reorder these */}
      {formattedRequirements.map((r, i) => (
        <ProgramRequirementDisplay
          requirement={r}
          key={i}
          storeKey={`${storeKeyPrefix}-${i}`}
          takenCourseIDs={takenCourseSet}
          transferredCourseIDs={transferredCourseSet}
        />
      ))}
    </div>
  );
};

export default ProgramRequirementsList;
