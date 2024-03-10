import { useDispatch, useSelector } from 'react-redux';
import ViewModeSelector from '../../../../components/search-form-components/ViewModeSelector';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';

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
  const setQueryParameter = useQueryParameter();

  const updateViewSubject = (viewSubjectValue) => {
    dispatch(setViewSubject(viewSubjectValue));
    setQueryParameter('view', viewSubjectValue);
  };

  const updateViewMode = (viewModeValue) => {
    dispatch(setViewMode(viewModeValue));
    setQueryParameter('mode', viewModeValue);
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
