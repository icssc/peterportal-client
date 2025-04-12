import { FC } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';

const UncategorizedCreditsSection: FC = () => {
  return (
    <MenuSection title="Other Transferred Credits">
      <SectionDescription>
        These items were not automatically recognized as a course or AP Exam. Once you add equivalent credits manually,
        you can remove them.
      </SectionDescription>
      Other Transfers Section Content
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
