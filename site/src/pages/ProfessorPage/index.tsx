import { FC, useState, useEffect } from 'react';
import './ProfessorPage.scss';
import { useParams } from 'react-router-dom';
import LoadingPage from '../LoadingPage';
import Twemoji from 'react-twemoji';
import { Divider } from 'semantic-ui-react';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import GradeDist from '../../component/GradeDist/GradeDist';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';

import { setProfessor } from '../../store/slices/popupSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { ProfessorGQLData } from '../../types/types';
import { searchAPIResult } from '../../helpers/util';

const ProfessorPage: FC = () => {
  const { id } = useParams<{ id: string }>();
  const dispatch = useAppDispatch();
  const professorGQLData = useAppSelector((state) => state.popup.professor);
  const [error, setError] = useState('');

  useEffect(() => {
    if (id !== undefined) {
      searchAPIResult('professor', id).then((professor) => {
        if (professor) {
          dispatch(setProfessor(professor as ProfessorGQLData));
          setError('');
        } else {
          setError(`Professor ${id} does not exist!`);
        }
      });
    }
  }, [dispatch, id]);

  const unionTerms = (courseHistory: Record<string, string[]>) => {
    // quarters mapped to the order of when they occur in the calendar year
    const quartersOrdered: Record<string, string> = {
      Winter: 'a',
      Spring: 'b',
      Summer1: 'c',
      Summer2: 'd',
      Summer10wk: 'e',
      Fall: 'f',
    };

    // get array of arrays of term names
    const allTerms = Object.values(courseHistory);

    // flatten and take union of array
    const union = [...new Set(allTerms.flat())];

    // sort so that the most recent term appears first in the dropdown
    union.sort((a, b) => {
      const [yearA, qtrA]: string[] = a.split(' ');
      const [yearB, qtrB]: string[] = b.split(' ');
      // first compare years (descending)
      // if years are equal, compare terms (most recent first)
      return yearB.localeCompare(yearA) || quartersOrdered[qtrB].localeCompare(quartersOrdered[qtrA]);
    });

    return union;
  };

  // if professor does not exists
  if (error) {
    return <Error message={error} />;
  }
  // loading results
  else if (!professorGQLData) {
    return <LoadingPage />;
  } else {
    return (
      <Twemoji options={{ className: 'twemoji' }}>
        <div className="professor-page">
          <div>
            <SideInfo
              searchType="professor"
              name={professorGQLData.name}
              title={professorGQLData.title}
              school={professorGQLData.schools[0]}
              description={professorGQLData.department}
              tags={[professorGQLData.ucinetid, professorGQLData.shortenedName]}
              professor={professorGQLData}
            />
          </div>
          <article className="professor-page-body">
            <div className="professor-page-section">
              <div>
                <h2>🗓️ Schedule of Classes</h2>
              </div>
              <Divider />
              <Schedule
                professorID={professorGQLData.shortenedName}
                termsOffered={unionTerms(professorGQLData.courseHistory)}
              />
            </div>

            <div className="professor-page-section">
              <div>
                <h2>📊 Grade Distribution</h2>
              </div>
              <Divider />
              <GradeDist professor={professorGQLData} />
            </div>

            <div className="professor-page-section">
              <div>
                <h2>💬 Reviews</h2>
              </div>
              <Divider />
              <Review professor={professorGQLData} />
            </div>
          </article>
        </div>
      </Twemoji>
    );
  }
};

export default ProfessorPage;
