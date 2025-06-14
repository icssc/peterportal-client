import { FC, useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';

import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import GradeDist from '../../component/GradeDist/GradeDist';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import ResultPageContent from '../../component/ResultPageContent/ResultPageContent';

import { setProfessor } from '../../store/slices/popupSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { searchAPIResult } from '../../helpers/util';

const ProfessorPage: FC = () => {
  const { id } = useParams<{ id: string }>();
  const dispatch = useAppDispatch();
  const professorGQLData = useAppSelector((state) => state.popup.professor);
  const [error, setError] = useState('');

  useEffect(() => {
    if (!id) return;

    searchAPIResult('professor', id).then((professor) => {
      if (!professor) {
        setError(`Professor ${id} does not exist!`);
        return;
      }
      dispatch(setProfessor(professor));
      setError('');
      document.title = `${professor.name} | PeterPortal`;
    });
  }, [dispatch, id]);

  // if professor does not exists
  if (error) {
    return <Error message={error} />;
  }

  // loading results
  if (!professorGQLData) {
    return <LoadingSpinner />;
  }

  const sideInfo = (
    <SideInfo
      dataType="professor"
      data={professorGQLData}
      name={professorGQLData.name}
      title={professorGQLData.title}
      description={professorGQLData.department}
      tags={[professorGQLData.ucinetid, ...professorGQLData.shortenedNames]}
    />
  );

  const mainSections: {
    title: string;
    Component: JSX.Element;
  }[] = [
    { title: '📊 Grade Distribution', Component: <GradeDist dataType="professor" data={professorGQLData} /> },
    { title: '🗓️ Schedule of Classes', Component: <Schedule dataType="professor" data={professorGQLData} /> },
    { title: '💬 Reviews', Component: <Review dataType="professor" data={professorGQLData} /> },
  ];

  return <ResultPageContent sideInfo={sideInfo} mainSections={mainSections} />;
};

export default ProfessorPage;
