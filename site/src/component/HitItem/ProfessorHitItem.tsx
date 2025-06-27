import { FC } from 'react';
import './HitItem.scss';
import { Link, useNavigate } from 'react-router-dom';

import { ProfessorGQLData } from '../../types/types';
import { setProfessor } from '../../store/slices/popupSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { useIsMobile, removeWhitespace } from '../../helpers/util';

interface ProfessorHitItemProps {
  professor: ProfessorGQLData;
}

const ProfessorHitItem: FC<ProfessorHitItemProps> = ({ professor }) => {
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const isMobile = useIsMobile();
  const activeProfessor = useAppSelector((state) => state.popup.professor);

  const onClickName = () => {
    // set the professor popup
    dispatch(setProfessor(professor));
    // if click on a professor that is already in popup or if on mobile
    if ((activeProfessor && professor.ucinetid == activeProfessor.ucinetid) || isMobile) {
      navigate(`/professor/${professor.ucinetid}`);
    }
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  const initialsText = professor.name
    .split(' ')
    .map((x: string) => x[0])
    .join('');
  const initialsSize = 22 - (initialsText.length - 2) * 4;

  return (
    <div className="hit-item professor-hit" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="name-container">
        <div className="name-icon" style={{ fontSize: initialsSize }}>
          {initialsText}
        </div>
        <div>
          <p className="hit-name">{professor.name}</p>
          <p className="hit-subtitle">
            {professor.department} • {professor.title}
          </p>
        </div>
      </div>
      {Object.keys(professor.courses).length > 0 && (
        <div>
          <p>
            <b>Recently taught: </b>
            {Object.keys(professor.courses).map((item: string, index: number) => {
              return (
                <span key={`professor-hit-item-course-${index}`}>
                  {index ? ', ' : ''}
                  <Link to={'/course/' + removeWhitespace(item)} onClick={(e) => e.stopPropagation()}>
                    {item}
                  </Link>
                </span>
              );
            })}
          </p>
        </div>
      )}
    </div>
  );
};

export default ProfessorHitItem;
