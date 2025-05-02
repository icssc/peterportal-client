import React, { FC, useContext, useEffect, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import trpc from '../../../trpc';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import ThemeContext from '../../../style/theme-context';
import AsyncSelect from 'react-select/async';
import { CourseAAPIResponse } from '@peterportal/types';

interface TransferredCourse {
  courseName: string;
  units: number;
}

interface CourseSelectOption {
  value: TransferredCourse;
  label: string;
}

interface CourseCreditMenuTileProps {
  course: TransferredCourse;
  setCourses: React.Dispatch<React.SetStateAction<TransferredCourse[]>>;
}
const CourseCreditMenuTile: FC<CourseCreditMenuTileProps> = ({ course, setCourses }) => {
  const [units, setUnits] = useState<number>(course.units);

  const deleteFn = () => {
    trpc.transferCredits.removeTransferredCourse.mutate(course.courseName);
    setCourses((courses) => courses.filter((c) => c.courseName !== course.courseName));
  };

  return <MenuTile title={course.courseName} units={units} setUnits={setUnits} deleteFn={deleteFn} />;
};

const CoursesSection: FC = () => {
  const [courses, setCourses] = useState<TransferredCourse[]>([]);
  const [timeout, setTimeout] = useState<number | null>(null);
  const [abortFn, setAbortFn] = useState<() => void>();

  const isDark = useContext(ThemeContext).darkMode;

  const cancelIncompleteSearch = () => {
    abortFn?.();
    if (timeout) clearTimeout(timeout);
  };

  const loadCourses = async (query: string) => {
    const response = await trpc.search.get.query({ query, skip: 0, take: 10, resultType: 'course' });
    const courses = response.results.map((c) => c.result) as CourseAAPIResponse[];
    const options: CourseSelectOption[] = courses.map((c) => ({
      value: { courseName: c.id, units: c.maxUnits },
      label: c.id,
    }));
    return options;
  };

  const loadAfterTimeout = async (query: string) => {
    if (timeout) clearTimeout(timeout);
    return new Promise<CourseSelectOption[]>((resolve) => {
      const initSearch = async () => {
        const abortPromise = new Promise<null>((resolve) => setAbortFn(() => resolve));
        const response = await Promise.race([abortPromise, loadCourses(query)]);
        if (response) resolve(response);
      };
      setTimeout(window.setTimeout(initSearch, 300));
    });
  };

  const addCourse = (course: TransferredCourse) => {
    setCourses([...courses, course]);
    trpc.transferCredits.addTransferredCourse.mutate(course);
  };

  useEffect(() => {
    trpc.transferCredits.getTransferredCourses.query().then(setCourses);
  }, []);

  return (
    <MenuSection title="Courses You've Transferred">
      <SectionDescription>
        Enter courses you&rsquo;ve claimed credit for through a{' '}
        <a href="https://testingcenter.uci.edu/programs/placement-testing/" target="_blank" rel="noreferrer">
          credit by exam
        </a>{' '}
        or{' '}
        <a href="https://assist.org" target="_blank" rel="noreferrer">
          through another college
        </a>
        .
      </SectionDescription>

      {courses.map((course) => (
        <CourseCreditMenuTile key={course.courseName} course={course} setCourses={setCourses} />
      ))}

      <AsyncSelect
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Search for a course to add..."
        theme={(t) => comboboxTheme(t, isDark)}
        cacheOptions
        onInputChange={cancelIncompleteSearch}
        loadOptions={loadAfterTimeout}
        onChange={(option) => addCourse(option!.value)}
        value={null}
      />
    </MenuSection>
  );
};

export default CoursesSection;
