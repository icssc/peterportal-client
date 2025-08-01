'use client';
import { FC, useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import GradeDist from '../../component/GradeDist/GradeDist';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';

import { setProfessor } from '../../store/slices/popupSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { searchAPIResult, unionTerms, sortTerms } from '../../helpers/util';
import { getProfessorTerms } from '../../helpers/reviews';
import ResultPageContent, { ResultPageSection } from '../../component/ResultPageContent/ResultPageContent';

const ProfessorPage: FC = () => {
  const { id } = useParams<{ id: string }>();
  const dispatch = useAppDispatch();
  const professorGQLData = useAppSelector((state) => state.popup.professor);
  const [error, setError] = useState('');

  useEffect(() => {
    if (id !== undefined) {
      searchAPIResult('professor', id).then((professor) => {
        if (professor) {
          dispatch(setProfessor(professor));
          setError('');
          document.title = `${professor.name} | PeterPortal`;
        } else {
          setError(`Professor ${id} does not exist!`);
        }
      });
    }
  }, [dispatch, id]);

  // if professor does not exists
  if (error) {
    return <Error message={error} />;
  }
  // loading results
  else if (!professorGQLData) {
    return <LoadingSpinner />;
  } else {
    const sideInfo = (
      <SideInfo
        searchType="professor"
        name={professorGQLData.name}
        title={professorGQLData.title}
        description={professorGQLData.department}
        tags={[professorGQLData.ucinetid, ...professorGQLData.shortenedNames]}
        professor={professorGQLData}
      />
    );
    return (
      <ResultPageContent sideInfo={sideInfo}>
        <ResultPageSection title="📊 Grade Distribution">
          <GradeDist professor={professorGQLData} />
        </ResultPageSection>

        <ResultPageSection title="🗓️ Schedule of Classes">
          <Schedule
            professorIDs={professorGQLData.shortenedNames}
            termsOffered={unionTerms(professorGQLData.courses)}
          />
        </ResultPageSection>

        <ResultPageSection title="💬 Reviews">
          <Review professor={professorGQLData} terms={sortTerms(getProfessorTerms(professorGQLData))} />
        </ResultPageSection>
      </ResultPageContent>
    );
  }
};

export default ProfessorPage;
