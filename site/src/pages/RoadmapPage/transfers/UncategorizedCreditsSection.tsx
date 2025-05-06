import { FC, useEffect, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import trpc from '../../../trpc';

interface UncategorizedCourseEntry {
  name: string | null;
  units: number | null;
}

const UncategorizedMenuTile: FC<UncategorizedCourseEntry> = ({ name, units }) => {
  const deleteFn = () => {
    trpc.transferCredits.removeUncategorizedCourse.mutate({ name, units });
    // something redux filter here
  };

  return <MenuTile title={name ?? ''} units={units ?? 0} deleteFn={deleteFn} />;
};

const UncategorizedCreditsSection: FC = () => {
  const [courses, setCourses] = useState<UncategorizedCourseEntry[]>([]);

  useEffect(() => {
    trpc.transferCredits.getUncategorizedTransfers.query().then((response) => {
      setCourses(response);
    });
  }, []);

  if (courses.length === 0) {
    return null;
  }

  return (
    <MenuSection title="Other Transferred Credits">
      <SectionDescription>
        These items were not automatically recognized as a course or AP Exam. Once you add equivalent credits manually,
        you can remove them.
      </SectionDescription>

      {courses.map((course) => (
        <UncategorizedMenuTile key={`${course.name}-${course.units}`} name={course.name} units={course.units} />
      ))}
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
