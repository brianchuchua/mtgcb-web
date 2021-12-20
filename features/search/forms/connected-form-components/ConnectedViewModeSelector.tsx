import { useDispatch, useSelector } from 'react-redux';
import ViewModeSelector from '../../../../components/search-form-components/ViewModeSelector';
import { RootState } from '../../../../redux/rootReducer';

interface ConnectedViewModeSelectorProps {
  showSubjectChangeSection?: boolean;
  reduxSlice: string;
  setViewMode: any;
  setViewSubject: any;
}

const ConnectedViewModeSelector: React.FC<ConnectedViewModeSelectorProps> = ({
  reduxSlice,
  setViewMode,
  setViewSubject,
  showSubjectChangeSection = true,
}) => {
  const dispatch = useDispatch();
  const { viewMode, viewSubject } = useSelector((state: RootState) => state[reduxSlice]);

  const updateViewSubject = (viewSubjectValue) => {
    dispatch(setViewSubject(viewSubjectValue));
  };

  const updateViewMode = (viewModeValue) => {
    dispatch(setViewMode(viewModeValue));
  };

  return (
    <ViewModeSelector
      showSubjectChangeSection={showSubjectChangeSection}
      viewMode={viewMode}
      viewSubject={viewSubject}
      setViewSubject={updateViewSubject}
      setViewMode={updateViewMode}
    />
  );
};

export default ConnectedViewModeSelector;
