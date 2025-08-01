'use client';

// Import Global Store
import { store } from '../../store/store';
import { Provider } from 'react-redux';
// import { PostHogProvider } from 'posthog-js/react';
import AppThemeProvider from '../AppThemeProvider/AppThemeProvider';
import { FC, PropsWithChildren } from 'react';
import { useLoadSavedCourses } from '../../hooks/savedCourses';

const UserDataLoader: FC = () => {
  useLoadSavedCourses();

  return null;
};

const AppProvider: FC<PropsWithChildren> = ({ children }) => {
  return (
    <Provider store={store}>
      {/* <PostHogProvider
      apiKey={import.meta?.env?.VITE_PUBLIC_POSTHOG_KEY ?? ''}
      options={{ api_host: import.meta?.env?.VITE_PUBLIC_POSTHOG_HOST ?? '', autocapture: true, enable_heatmaps: true }}
    > */}
      <UserDataLoader />
      <AppThemeProvider>{children}</AppThemeProvider>
      {/* </PostHogProvider> */}
    </Provider>
  );
};

export default AppProvider;
