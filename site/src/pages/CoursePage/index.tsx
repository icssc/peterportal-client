import { FC, useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';

import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';
import ResultPageContent, { ResultPageSection } from '../../component/ResultPageContent/ResultPageContent';

import { setCourse } from '../../store/slices/popupSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { getCourseTags, searchAPIResult, sortTerms } from '../../helpers/util';

const CoursePage: FC = () => {
  const { id } = useParams<{ id: string }>();
  const dispatch = useAppDispatch();
  const courseGQLData = useAppSelector((state) => state.popup.course);
  const [error, setError] = useState('');

  useEffect(() => {
    if (id !== undefined) {
      searchAPIResult('course', id).then((course) => {
        if (course) {
          dispatch(setCourse(course));
          setError('');
          document.title = `${course.department + ' ' + course.courseNumber} | PeterPortal`;
        } else {
          setError(`Course ${id} does not exist!`);
        }
      });
    }
  }, [dispatch, id]);

  // if course does not exists
  if (error) {
    return <Error message={error} />;
  }
  // loading results
  else if (!courseGQLData) {
    return <LoadingSpinner />;
  } else {
    const sideInfo = (
      <SideInfo
        searchType="course"
        name={courseGQLData.department + ' ' + courseGQLData.courseNumber}
        title={courseGQLData.title}
        description={courseGQLData.description}
        tags={getCourseTags(courseGQLData)}
        course={courseGQLData}
        terms={courseGQLData.terms}
      />
    );
    return (
      <ResultPageContent sideInfo={sideInfo}>
        <ResultPageSection title="📊 Grade Distribution">
          <GradeDist course={courseGQLData} />
        </ResultPageSection>

        <ResultPageSection title="🌲 Prerequisite Tree">
          <PrereqTree key={courseGQLData.id} {...courseGQLData} />
        </ResultPageSection>

        <ResultPageSection title="🗓️ Schedule of Classes">
          <Schedule
            key={courseGQLData.id}
            courseID={courseGQLData.department + ' ' + courseGQLData.courseNumber}
            termsOffered={sortTerms(courseGQLData.terms)}
          />
        </ResultPageSection>

        <ResultPageSection title="💬 Reviews">
          <Review key={courseGQLData.id} course={courseGQLData} terms={sortTerms(courseGQLData.terms)} />
        </ResultPageSection>
      </ResultPageContent>
    );
  }
};

export default CoursePage;
