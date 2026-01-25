import { FC } from 'react';
import './HitItem.scss';
import { useAppDispatch } from '../../store/hooks';

import { ProfessorGQLData } from '../../types/types';
import Link from 'next/link';
import { addPreview, clearPreviews } from '../../store/slices/previewSlice';

interface ProfessorHitItemProps extends ProfessorGQLData {}

const ProfessorHitItem: FC<ProfessorHitItemProps> = (props: ProfessorHitItemProps) => {
  const dispatch = useAppDispatch();
  const onClickName = () => {
    dispatch(clearPreviews());
    dispatch(addPreview({ type: 'professor', id: props.ucinetid }));
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  const initialsText = props.name
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
          <p className="hit-name">{props.name}</p>
          <p className="hit-subtitle">
            {props.department}&nbsp;• {props.title}
          </p>
        </div>
      </div>
      {Object.keys(props.courses).length > 0 && (
        <div>
          <p>
            <b>Recently taught: </b>
            {Object.keys(props.courses).map((item: string, index: number) => {
              const handleLinkClick = (event: React.MouseEvent) => {
                event.preventDefault();
                event.stopPropagation();
                dispatch(clearPreviews());
                dispatch(addPreview({ type: 'course', id: item }));
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
