import { FC } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';

const CoursesSection: FC = () => {
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
      Courses Section Content
    </MenuSection>
  );
};

export default CoursesSection;
