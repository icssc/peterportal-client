const UIOverlay = ({ zIndex, ...props }: { zIndex: number } & JSX.IntrinsicElements['div']) => {
  // Clicking this is only an alternative action to something that is already accessible
  // eslint-disable-next-line jsx-a11y/no-static-element-interactions,jsx-a11y/click-events-have-key-events
  return <div className="ui-overlay" {...props} style={{ zIndex }}></div>;
};

export default UIOverlay;
