import { FC, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';

/** @todo retrieve all uncategorized transfer credits */

const UncategorizedMenuTile: FC = () => {
  const [units, setUnits] = useState<number>(0);

  const deleteFn = () => {};

  /** @todo router to remove individual transfer courses */

  return <MenuTile title="Name" units={units} setUnits={setUnits} deleteFn={deleteFn}></MenuTile>;
};

const UncategorizedCreditsSection: FC = () => {
  return (
    <MenuSection title="Other Transferred Credits">
      <SectionDescription>
        These items were not automatically recognized as a course or AP Exam. Once you add equivalent credits manually,
        you can remove them.
      </SectionDescription>

      {/** @todo map each item in array with component */}

      <UncategorizedMenuTile />
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
