import { FC } from 'react';
import './HitItem.scss';
import { useAppDispatch } from '../../store/hooks';

import { ProfessorGQLData } from '../../types/types';
import Link from 'next/link';
import { setPreviewedCourse, setPreviewedProfessor } from '../../store/slices/coursePreviewSlice';

interface ProfessorHitItemProps extends ProfessorGQLData {}

const ProfessorHitItem: FC<ProfessorHitItemProps> = (props: ProfessorHitItemProps) => {
  const dispatch = useAppDispatch();

  const onClickName = () => {
    dispatch(setPreviewedProfessor(props.ucinetid));
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  return (
    <div className="hit-item professor-hit" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="name-container">
        <div>
          <p className="hit-name">{props.name}</p>
          <p className="hit-subtitle">
            {props.title && <span className="prof-title">{props.title}</span>}

            {props.title && props.department && ' • '}

            {props.department && <span className="prof-department">{props.department}</span>}
          </p>
        </div>
      </div>
      {Object.keys(props.courses).length > 0 && (
        <div>
          <p>
            <b>Recently Taught: </b>
            {Object.keys(props.courses).map((item: string, index: number) => {
              const handleLinkClick = (event: React.MouseEvent) => {
                event.preventDefault();
                event.stopPropagation();
                dispatch(setPreviewedCourse(item));
              };

              return (
                <span key={`professor-hit-item-course-${index}`}>
                  {index ? ', ' : ''}
                  <Link href={'/course/' + encodeURIComponent(item.replace(/\s+/g, ''))} onClick={handleLinkClick}>
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
