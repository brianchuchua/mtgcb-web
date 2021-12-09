import { useDispatch, useSelector } from 'react-redux';
import ViewModeSelector from '../../../../components/search-form-components/ViewModeSelector';
import { RootState } from '../../../../redux/rootReducer';
import { setViewMode, setViewSubject } from '../../browseSlice';

interface ConnectedViewModeSelectorProps {
  showSubjectChangeSection?: boolean;
}
const ConnectedViewModeSelector: React.FC<ConnectedViewModeSelectorProps> = ({ showSubjectChangeSection = true }) => {
  const dispatch = useDispatch();
  const { viewMode, viewSubject } = useSelector((state: RootState) => state.browse);

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
