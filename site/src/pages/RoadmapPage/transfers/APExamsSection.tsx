import { FC } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';

const APExamsSection: FC = () => {
  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      AP Section Content
    </MenuSection>
  );
};

export default APExamsSection;
