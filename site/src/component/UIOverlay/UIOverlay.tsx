import { FC } from 'react';

interface UIOverlayProps {
  zIndex: number;
  passedRef?: React.RefObject<HTMLDivElement>;
}

const UIOverlay: FC<UIOverlayProps & JSX.IntrinsicElements['div']> = ({ zIndex, ...props }) => {
  const passedRef = props.passedRef;
  delete props.passedRef;
  // Clicking this is only an alternative action to something that is already accessible
  return <div className="ui-overlay" {...props} ref={passedRef} style={{ zIndex }} />;
};

export default UIOverlay;
