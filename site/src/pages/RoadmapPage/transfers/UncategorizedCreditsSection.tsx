import { FC, useEffect, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import trpc from '../../../trpc';

/** @todo retrieve all uncategorized transfer credits */

interface UncategorizedCreditsEntry {
  name: string | null;
  units: number | null;
}

const UncategorizedMenuTile: FC<UncategorizedCreditsEntry> = ({ name, units }) => {
  const deleteFn = () => {};

  /** @todo router to remove individual transfer courses */
  // delete something where ctx user and name and units

  return <MenuTile title={name ?? ''} units={units ?? 0} deleteFn={deleteFn}></MenuTile>;
};

const UncategorizedCreditsSection: FC = () => {
  const [courses, setCourses] = useState<UncategorizedCreditsEntry[]>([]);

  useEffect(() => {
    trpc.transferCredits.getUncategorizedTransfers.query().then((response) => {
      setCourses(response);
    });
  }, []);

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
