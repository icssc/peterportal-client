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
import { searchAPIResult, unionTerms } from '../../helpers/util';

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
              description={professorGQLData.department}
              tags={[professorGQLData.ucinetid, ...professorGQLData.shortenedNames]}
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
                professorIDs={professorGQLData.shortenedNames}
                termsOffered={unionTerms(professorGQLData.courses)}
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
